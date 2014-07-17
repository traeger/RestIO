-----------------------------------------------------------------------------
-- |
-- Module      :  RestIO
-- Copyright   :  (c) 2014 Marco Traeger (marco.traeger@googlemail.com)
-- License     :  MIT License (see http://opensource.org/licenses/MIT)
--
-- Maintainer  :  marco.traeger@googlemail.com 
-- Stability   :  dev
--
-- Standard IO via a Restful Interface
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module RestIO
  ( Port, Buffersize
  , start
  ) where

import Control.Exception (bracket)
import System.Process
import GHC.IO.Handle
import qualified RestIO.StrictIO as StrictIO
import qualified System.IO as SystemIO
import Control.Monad
import Control.Concurrent

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Blaze.ByteString.Builder as Blaze ( fromByteString )
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze ( fromString )
import Network.Wai.UrlMap
import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as Char8

import qualified RestIO.HistoryBuffer as HistoryBuffer
import RestIO.HistoryBuffer (HistoryBuffer)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe

type Port = Int
type Buffersize = Int

start :: String -> Buffersize -> Port -> IO ()
start cmd buffersize port = do
  putStrLn $ "running on http://localhost:" ++ (show port) ++ "/"
  buffer <- HistoryBuffer.empty buffersize ""
  bracket
    (do
      r@(pin, pout, perr, pid) <- runInteractiveCommandTextmode cmd
      forkIO $ forever $ do
        s <- StrictIO.getLineStrict pout
        HistoryBuffer.push buffer $ s
        putStrLn $ "to outer:" ++ s -- debug
      return r
    ) -- IO a, where a = (Handle, Handle, Handle, ProcessHandle)
    (\(_,_,_,pid) -> terminateProcess pid) -- a -> IO ()
    (\(pin, pout, perr, _) -> run port $ app buffer (pin, pout, perr)) -- a -> IO ()
  
app :: HistoryBuffer String -> (Handle, Handle, Handle) -> Application
app buffer (pin, pout, perr) = mapUrls $
      mount "echo"  (echoApp)
  <|> mount "stdin" (stdinApp pin)
  <|> mount "stdout" (stdoutApp buffer)
  <|> mount "client" (fileApp)

-- | Echo Service
echoApp :: Application
echoApp request respond = respond $
  responseStream status200 [] $ \write flush -> do
    text <- requestBody request
    putStrLn $ "echo: " ++ (Char8.unpack text)
    write $ Blaze.fromByteString text
    flush
    
-- | Serves files in the client subdirectory
fileApp :: Application
fileApp request respond = respond $
--  responseStream status200 [] $ \write flush -> do
--    write $ fromByteString $ rawPathInfo request
--    flush
  responseFile status200 [] ("client" ++ (Char8.unpack $ rawPathInfo request)) Nothing

-- | Forward the request body to the stdin-handle
stdinApp :: Handle -> Application
stdinApp pin request respond = respond $ 
  responseStream status200 [] $ \write flush -> do
    text <- requestBody request
    putStrLn $ "debug in: " ++ (Char8.unpack text) -- debug
    SystemIO.hPutStrLn pin $ (Char8.unpack text)
    --write $ fromByteString text -- like a echo service
    
-- | Forward the buffered stdout and stderr to the response body
stdoutApp :: HistoryBuffer String -> Application
stdoutApp buffer request respond = respond $ 
  responseStream status200 [] $ \write flush -> do
    let q = Map.lookup (Char8.pack "last") $ query request
    (last, xs) <- if Maybe.isJust q
      then do
        let q' = Char8.unpack $ Maybe.fromJust q
        putStrLn q'
        HistoryBuffer.read buffer $ (read q' :: Int)
      else do
        HistoryBuffer.read buffer (-1)
    write $ Blaze.fromString $ (show last) ++ "\n"
    (flip mapM_) xs $ \x -> do
      write $ Blaze.fromString $ x ++ "\n"
      flush

-----------------------------------------------------------------------------
-- Helper

query :: Request -> Map Char8.ByteString Char8.ByteString
query r = Map.fromList $ Maybe.catMaybes $ map pullmaybe $ queryString r where
  pullmaybe (_, Nothing) = Nothing
  pullmaybe (x, Just y) = Just (x, y)

-- | Like runInteractiveCommand but with all Handle in textmode
runInteractiveCommandTextmode :: String -> IO (Handle, Handle, Handle, ProcessHandle)
runInteractiveCommandTextmode cmd = do
  r@(pin, pout, perr, pid) <- runInteractiveCommand cmd
  hSetBinaryMode pin False
  hSetBinaryMode pout False
  hSetBinaryMode perr False
  hSetBuffering pin NoBuffering
  hSetBuffering pout NoBuffering
  hSetBuffering perr NoBuffering
  return r
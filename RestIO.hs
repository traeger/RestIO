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

module RestIO where

import System.Process
import Control.Exception (bracket)
import GHC.IO.Handle

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Blaze.ByteString.Builder (fromByteString)
import Network.Wai.UrlMap
import Control.Applicative ((<|>))
import qualified Data.ByteString.Char8 as Char8

import RestIO.HistoryBuffer

type Port = Int

main = putStrLn "http://localhost:3000/" >> (start 3000 "")

start :: Port -> String -> IO ()
start port cmd = bracket
  (runInteractiveCommandTextmode cmd) -- IO a, where a = (Handle, Handle, Handle, ProcessHandle)
  (\(_,_,_,pid) -> terminateProcess pid) -- a -> IO ()
  (\(stdin, stdout, stderr, _) -> run port $ app undefined (stdin, stdout, stderr)) -- a -> IO ()
  
app :: HistoryBuffer e -> (Handle, Handle, Handle) -> Application
app buffer (stdin, stdout, stderr) = mapUrls $
      mount "echo"  (echoApp)
  <|> mount "stdin" (stdinApp stdin)
  <|> mount "stdout" (echoApp) --(stdoutApp buffer stdout stderr)
  <|> mount "client" (fileApp)

-- | Echo Service
echoApp :: Application
echoApp request respond = respond $
  responseStream status200 [] $ \write flush -> do
    text <- requestBody request
    putStrLn $ "echo: " ++ (Char8.unpack text)
    write $ fromByteString text
    flush
    
-- | Serves files in the client subdirectory
fileApp :: Application
fileApp request respond = respond $
--  responseStream status200 [] $ \write flush -> do
--    write $ fromByteString $ rawPathInfo request
--    flush
  responseFile status200 [] ("client" ++ (Char8.unpack $ rawPathInfo request)) Nothing

-- | Forward the request body to the stdin-handle
stdinApp :: (Handle) -> Application
stdinApp stdin request respond = respond $ 
  responseStream status200 [] $ \write flush -> do
    text <- requestBody request
    putStrLn $ "in: " ++ (Char8.unpack text)
    write $ fromByteString text
    
-- | Forward the buffered stdout and stderr to the response body
stdoutApp :: HistoryBuffer e -> (Handle) -> Application
stdoutApp buffer (stdin) request respond = respond $ 
  responseStream status200 [] $ \write flush -> do
    write $ fromByteString "std out 1\n"
    flush
    write $ fromByteString "std out 2\n"

-----------------------------------------------------------------------------
-- Helper

-- | Like runInteractiveCommand but with all Handle in textmode
runInteractiveCommandTextmode :: String -> IO (Handle, Handle, Handle, ProcessHandle)
runInteractiveCommandTextmode cmd = do
  r@(stdin, stdout, stderr, pid) <- runInteractiveCommand cmd
  hSetBinaryMode stdin False
  hSetBinaryMode stdout False
  hSetBinaryMode stderr False
  return r
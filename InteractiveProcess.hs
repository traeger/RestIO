-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2014 Marco Traeger (marco.traeger@googlemail.com)
-- License     :  MIT License (see http://opensource.org/licenses/MIT)
--
-- Maintainer  :  marco.traeger@googlemail.com 
-- Stability   :  dev
--
-- Standard IO via a Restful Interface
--
-----------------------------------------------------------------------------

import System.Process
import GHC.IO.Handle
import qualified RestIO.StrictIO as StrictIO
import qualified System.IO as SystemIO
import Control.Monad
import Control.Concurrent

main :: IO()
main = do
  r@(pin, pout, perr, pid) <- runInteractiveCommandTextmode "echo.exe"
  forkIO $ forever $ do
    s <- StrictIO.getLineStrict pout
    putStrLn $ "to outer:" ++ s
  forever $ do
    s <- StrictIO.getLineStrict SystemIO.stdin
    SystemIO.hPutStrLn pin $ "to inner:" ++ s
  
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
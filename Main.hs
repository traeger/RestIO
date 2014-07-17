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

{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

import qualified RestIO
import System.Console.CmdArgs

data Parameters = Parameters {cmd :: String, buffersize :: Int, port :: Int} deriving (Data,Typeable,Show)

parameters = cmdArgsMode $ Parameters
    {cmd = ""  &= argPos 0 &= typ "CMD"
    ,buffersize = 50 &= name "b" &= help "Buffersize (in lines) for the stdout of the Process."
    ,port = 3000 &= name "p" &= help "Port where the Restservice is accessable."}
    &= summary "Standard IO via a Restful Interface. Runs the Command CMD and forwards all IO using the Restful Interface. Accessable via [host:port]/client/index.html"

main :: IO()
main = do
  Parameters{..} <- cmdArgsRun parameters
  RestIO.start cmd buffersize port

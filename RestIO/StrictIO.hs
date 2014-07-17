-----------------------------------------------------------------------------
-- |
-- Module      :  RestIO.StrictIO
-- Copyright   :  (c) 2014 Marco Traeger (marco.traeger@googlemail.com)
-- License     :  MIT License (see http://opensource.org/licenses/MIT)
--
-- Maintainer  :  marco.traeger@googlemail.com 
-- Stability   :  experimental
--
-- Strict IO.
--
-----------------------------------------------------------------------------

module RestIO.StrictIO where

import System.IO
import GHC.IO.Handle

getLineStrict :: Handle -> IO String
getLineStrict h = hGetLine h >>= \s -> length s `seq` return s
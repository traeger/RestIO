-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2014 Marco Traeger (marco.traeger@googlemail.com)
-- License     :  MIT License (see http://opensource.org/licenses/MIT)
--
-- Maintainer  :  marco.traeger@googlemail.com 
-- Stability   :  dev
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Control.Exception.Base (bracket_)

import RestIO.HistoryBuffer

app :: HistoryBuffer e -> Application
app buffer request respond = respond $ case pathInfo request of
  ["echo"] -> responseLBS status200 [("Content-Type", "text/plain")] "as"
  _        -> responseFile status404 [] "Response/404" Nothing

--app :: Application
--app req respond = respond $ responseLBS status200 [] "Hello"

main = putStrLn "http://localhost:3001/" >> (run 3000 $ app undefined)
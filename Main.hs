-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2014 Marco Traeger (marco.traeger@googlemail.com)
-- License     :  MIT License (see http://opensource.org/licenses/MIT)
--
-- Maintainer  :  marco.traeger@googlemail.com 
-- Stability   :  dev
--
-----------------------------------------------------------------------------

--{-# LANGUAGE OverloadedStrings #-}
--import Web.Scotty

--import Network.Wai.Middleware.RequestLogger -- install wai-extra if you don't have this

--import Data.Monoid (mconcat)

--main = scotty 3000 $ do
--  --middleware logStdoutDev

--  get "/:word" $ do
--    beam <- param "word"
--    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import RestIO.HistoryBuffer

--app :: HistoryBuffer e -> Application
--app buffer request = return $ case pathInfo request of
--  ["echo"] -> responseLBS status200 [("Content-Type", "text/plain")] "hello world"
--  _        -> responseFile status404 [] "Response/404" Nothing

app :: Application
app req respond = respond $ responseLBS status200 [] "Hello"

main = putStrLn "http://localhost:3001/" >> (run 3000 $ app)
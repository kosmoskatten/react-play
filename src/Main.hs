{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Main
    ( main
    ) where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Proxy (..), Raw, Server, serve, serveDirectory)

type API = Raw

api :: Proxy API
api = Proxy

server :: Server API
server = serveDirectory "static"

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app

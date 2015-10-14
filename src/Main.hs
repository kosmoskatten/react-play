{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main
    ( main
    ) where

import Control.Monad.Trans.Either (EitherT)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant

data Comment =
    Comment { author :: !Text
            , text   :: !Text
            }
    deriving (Generic, Show)

instance ToJSON Comment

type API = "api" :> "comments" :> Get '[JSON] [Comment] 
      :<|> Raw

api :: Proxy API
api = Proxy

comments :: EitherT ServantErr IO [Comment]
comments = return [ Comment { author = "Sigge Pigg"
                            , text = "A little meow" }
                  , Comment { author = "Frasse Tass"
                            , text = "Just *another* meow" }
                  ]

server :: Server API
server = comments
    :<|> serveDirectory "static"

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main
    ( main
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (EitherT)
import Data.Aeson (FromJSON, ToJSON)
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

instance FromJSON Comment
instance ToJSON Comment

type API = "api" :> "comments" :> Get '[JSON] [Comment]
      :<|> "api" :> "comments" :> ReqBody '[JSON] Comment
                               :> Post '[JSON] [Comment]
      :<|> Raw

api :: Proxy API
api = Proxy

getComments :: EitherT ServantErr IO [Comment]
getComments = do
    liftIO $ putStrLn "Fetching comments ..."
    return [ Comment { author = "Sigge Pigg"                        
                     , text = "A little meow" }
           , Comment { author = "Frasse Tass"
                     , text = "Just *another* meow" }
           ]

postComment :: Comment -> EitherT ServantErr IO [Comment]
postComment comment = do
    liftIO $ putStrLn ("Got " ++ (show comment))
    return []

server :: Server API
server = getComments
    :<|> postComment
    :<|> serveDirectory "static"

app :: Application
app = serve api server

main :: IO ()
main = run 8081 app

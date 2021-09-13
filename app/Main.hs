module Main where

import           Network.Wai
import           Relude
import           Servant

import           ApiType

server :: Server API
server = getUsersH :<|> addUserH
  where
    getUsersH = return [User 0 ""]
    addUserH _ = return (User 0 "")

app :: Application
app = serve api server

main :: IO ()
main = do
  pass

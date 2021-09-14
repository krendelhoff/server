module Main where

import qualified Hasql.Connection         as Conn
import           Hasql.Pool               (Pool, use)
import qualified Hasql.Pool               as Pool
import           Hasql.Session            (sql, statement)
import           Hasql.TH
import           Network.Wai.Handler.Warp (run)
import           Relude
import           Servant

import           ApiType
import           Database.API

connSettings :: Conn.Settings
connSettings = "host=localhost port=5432 dbname=db connect_timeout=10"

poolSettings :: Pool.Settings
poolSettings = (10, 5, connSettings)

server :: Pool -> Server API
server pool =
  (getUsersH :<|> addUserH) :<|> (getToolsH :<|> addToolH) :<|>
  (checkoutH :<|> checkinH :<|> checkedoutH :<|> checkedinH)
  where
    getUsersH = runReaderT getUsers pool
    addUserH Nothing         = throwError err400
    addUserH (Just username) = runReaderT (addUser username) pool
    getToolsH = runReaderT getTools pool
    addToolH Nothing _               = throwError err400
    addToolH _ Nothing               = throwError err400
    addToolH (Just name) (Just desc) = runReaderT (addTool name desc) pool
    checkoutH Nothing _             = throwError err400
    checkoutH _ Nothing             = throwError err400
    checkoutH (Just uid) (Just tid) = runReaderT (checkout uid tid) pool
    checkinH id = runReaderT (checkin id) pool
    checkedoutH = runReaderT getCheckedout pool
    checkedinH = runReaderT checkedin pool

app :: Pool -> Application
app = serve api . server

main :: IO ()
main = do
  withPool poolSettings $ run 8080 . app

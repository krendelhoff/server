module Main where

import           Control.Exception        (bracket)
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
server pool = getUsersH :<|> addUserH
  where
    getUsersH = runReaderT getUser pool
    addUserH Nothing         = throwError err400
    addUserH (Just username) = runReaderT (addUser username) pool

app :: Pool -> Application
app = serve api . server

withPool :: Pool.Settings -> (Pool -> IO ()) -> IO ()
withPool settings = bracket (Pool.acquire settings) Pool.release

main :: IO ()
main = do
  withPool poolSettings $ run 8080 . app

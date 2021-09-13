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

connSettings :: Conn.Settings
connSettings = "host=localhost port=5432 dbname=db connect_timeout=10"

poolSettings :: Pool.Settings
poolSettings = (10, 5, connSettings)

server :: Pool -> Server API
server pool = getUsersH :<|> addUserH
  where
    getUsersH = do
      result <-
        liftIO $
        use pool $
        statement
          ()
          [vectorStatement|select user_id :: int8, username :: text from users|]
      case result of
        Left err   -> return [User 0 ""]
        Right boob -> return $ toList . fmap (uncurry User) $ boob
    addUserH Nothing = throwError err400
    addUserH (Just username) = do
      result <-
        liftIO $
        use pool $
        statement
          username
          [resultlessStatement|insert into users (username) values ($1 :: text)|]
      case result of
        Left err   -> throwError err417
        Right boob -> pass

app :: Pool -> Application
app = serve api . server

withPool :: Pool.Settings -> (Pool -> IO ()) -> IO ()
withPool settings = bracket (Pool.acquire settings) Pool.release

main :: IO ()
main = do
  withPool poolSettings $ run 8080 . app

module Database.API
  ( getUser
  , addUser
  ) where

import           Hasql.Pool    (Pool, use)
import           Hasql.Session (statement)
import           Hasql.TH
import           Relude
import           Servant

import           ApiType

getUser :: ReaderT Pool Handler [User]
getUser = do
  pool <- ask
  result <-
    liftIO $
    use pool $
    statement
      ()
      [vectorStatement|select user_id :: int8, username :: text from users|]
  case result of
    Left err   -> return [User 0 ""]
    Right boob -> return $ toList . fmap (uncurry User) $ boob

addUser :: Text -> ReaderT Pool Handler ()
addUser username = do
  pool <- ask
  result <-
    liftIO $
    use pool $
    statement
      username
      [resultlessStatement|insert into users (username) values ($1 :: text)|]
  case result of
    Left err   -> throwError err417
    Right boob -> pass

module Database.API
  ( getUsers
  , addUser
  , withPool
  , addTool
  , getTools
  ) where

import           Control.Exception (bracket)
import           Data.Time         (getCurrentTime, utctDay)
import           Hasql.Pool        (Pool, Settings, acquire, release, use)
import           Hasql.Session     (statement)
import           Hasql.TH
import           Relude
import           Servant

import           ApiType

withPool :: Settings -> (Pool -> IO ()) -> IO ()
withPool settings = bracket (acquire settings) release

getUsers :: ReaderT Pool Handler Users
getUsers = do
  pool <- ask
  result <-
    liftIO $
    use pool $
    statement
      ()
      [vectorStatement|select user_id :: int8, username :: text from users|]
  case result of
    Left err   -> throwError err500
    Right boob -> return $ Users . fmap (uncurry User) $ boob

addUser :: Text -> ReaderT Pool Handler NoContent
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
    Right boob -> return NoContent

addTool :: Text -> Text -> ReaderT Pool Handler NoContent
addTool name desc = do
  pool <- ask
  today <- liftIO $ utctDay <$> getCurrentTime
  result <-
    liftIO $
    use pool $
    statement
      (name, desc, today)
      [resultlessStatement|insert into tools (name,description,lastTouched,timesBorrowed) values ($1 :: text, $2 :: text, $3 :: date, 0 :: int8)|]
  case result of
    Left err   -> throwError err417
    Right boob -> return NoContent

getTools :: ReaderT Pool Handler Tools
getTools = do
  pool <- ask
  result <-
    liftIO $
    use pool $
    statement
      ()
      [vectorStatement|select tool_id :: int8,name :: text,description :: text,lastTouched :: date,timesBorrowed :: int8 from tools|]
  case result of
    Left err   -> throwError err500 -- log err
    Right boob -> return $ Tools . fmap filler $ boob
  where
    filler (tool_id, name, desc, lT, tB) = Tool tool_id name desc lT tB

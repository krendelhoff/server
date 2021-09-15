module Database.API
  ( getUsers
  , addUser
  , withPool
  , addTool
  , getTools
  , checkin
  , checkedin
  , checkedout
  , checkout
  , getCheckedout
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

filler (tool_id, name, desc, lT, tB) = Tool tool_id name desc lT tB

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

getCheckedout :: ReaderT Pool Handler Checkedout
getCheckedout = do
  pool <- ask
  result <-
    liftIO $
    use pool $
    statement
      ()
      [vectorStatement|select user_id :: int8, tool_id :: int8 from checkedout|]
  case result of
    Left err   -> throwError err500
    Right boob -> return $ Checkedout . fmap (uncurry CheckedoutEntity) $ boob

checkout :: Int64 -> Int64 -> ReaderT Pool Handler NoContent
checkout uid tid = do
  pool <- ask
  result <-
    liftIO $
    use pool $
    statement
      (uid, tid)
      [resultlessStatement|insert into checkedout (user_id,tool_id) values ($1 :: int8, $2 :: int8)|]
  case result of
    Left err   -> throwError err417
    Right boob -> return NoContent

checkedin :: ReaderT Pool Handler Tools
checkedin = do
  pool <- ask
  result <-
    liftIO $
    use pool $
    statement
      ()
      [vectorStatement|select
                              tool_id :: int8
                            , name :: text
                            , description :: text
                            , lastTouched :: date
                            , timesBorrowed :: int8
                       from tools
                       where tool_id not in (select
                                                tool_id :: int8
                                             from checkedout)
      |]
  case result of
    Left err   -> throwError err417
    Right boob -> return $ Tools . fmap filler $ boob

checkin :: Int64 -> ReaderT Pool Handler NoContent
checkin id = do
  inCheckedout <- checkForCheckedout
  if inCheckedout
    then mainAction
    else throwError err400
  where
    mainAction = do
      deleteToolFromCheckedout
      tB <- selectTool
      updateTool tB
    checkForCheckedout = do
      pool <- ask
      result <-
        liftIO $
        use pool $
        statement
          id
          [singletonStatement|select count(tool_id) :: int8 from checkedout group by tool_id having tool_id = $1 :: int8|]
      case result of
        Left err -> throwError err417
        Right 0  -> return False
        _        -> return True
    deleteToolFromCheckedout = do
      pool <- ask
      result <-
        liftIO $
        use pool $
        statement
          id
          [resultlessStatement|delete from checkedout where tool_id = $1 :: int8|]
      case result of
        Left err -> throwError err417
        Right _  -> pass
    selectTool = do
      pool <- ask
      result <-
        liftIO $
        use pool $
        statement
          id
          [singletonStatement|select timesBorrowed :: int8
                                  from tools
                                  where tool_id = $1 :: int8
              |]
      case result of
        Left err -> throwError err417
        Right tB -> return tB
    updateTool tB = do
      pool <- ask
      today <- liftIO $ utctDay <$> getCurrentTime
      result <-
        liftIO $
        use pool $
        statement
          (today, tB + 1, id)
          [resultlessStatement|update tools set lastTouched = $1 :: date, timesBorrowed = $2 :: int8 where tool_id = $3 :: int8|]
      case result of
        Left err -> throwError err417
        Right _  -> return NoContent

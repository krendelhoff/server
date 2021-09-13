module ApiType
  ( API
  , api
  , User(..)
  , Tool(..)
  ) where

import           Data.Time   (Day)
import           Relude
import           Servant.API

type UserAPI
   = "users" :> (Get '[ JSON] [User] :<|> QueryParam "username" Text :> Post '[ JSON] User)

type ToolAPI
   = "tools" :> (Get '[ JSON] [Tool] :<|> QueryParam "name" Text :> QueryParam "desc" Text :> Post '[ JSON] Tool)

type API = UserAPI :<|> ToolAPI

api :: Proxy API
api = Proxy

data User =
  User
    { user_id  :: Int
    , username :: Text
    }

data Tool =
  Tool
    { tool_id       :: Int
    , name          :: Text
    , description   :: Text
    , lastReturned  :: Day
    , timesBorrowed :: Int
    }

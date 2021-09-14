module ApiType
  ( API
  , api
  , User(..)
  , Tool(..)
  , Checkedout(..)
  , Users(..)
  , Tools(..)
  ) where

import           Data.Aeson
import           Data.Time    (Day)
import           Data.Vector  (Vector)
import           GHC.Generics
import           Relude
import           Servant.API

type UserAPI
   = "users" :> (Get '[ JSON] Users :<|> QueryParam "username" Text :> Post '[ JSON] NoContent)

type ToolAPI
   = "tools" :> (Get '[ JSON] Tools :<|> QueryParam "name" Text :> QueryParam "desc" Text :> Post '[ JSON] NoContent)

type API = UserAPI :<|> ToolAPI

api :: Proxy API
api = Proxy

data User =
  User
    { user_id  :: Int64
    , username :: Text
    }
  deriving (Generic, ToJSON, FromJSON)

newtype Users =
  Users
    { users :: Vector User
    }
  deriving (Generic, ToJSON, FromJSON)

data Tool =
  Tool
    { tool_id       :: Int64
    , name          :: Text
    , description   :: Text
    , lastTouched   :: Day
    , timesBorrowed :: Int64
    }
  deriving (Generic, ToJSON, FromJSON)

newtype Tools =
  Tools
    { tools :: Vector Tool
    }
  deriving (Generic, ToJSON, FromJSON)

data Checkedout =
  Checkedout
    { user_id :: Int64
    , tool_id :: Int64
    }
  deriving (Generic, ToJSON, FromJSON)

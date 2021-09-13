module ApiType
  ( API
  , api
  , User(..)
  , Tool(..)
  ) where

import           Data.Aeson
import           Data.Time    (Day)
import           GHC.Generics
import           Relude
import           Servant.API

type UserAPI
   = "users" :> (Get '[ JSON] [User] :<|> QueryParam "username" Text :> Post '[ JSON] User)

type API = UserAPI

api :: Proxy API
api = Proxy

data User =
  User
    { user_id  :: Int
    , username :: Text
    }
  deriving (Generic, ToJSON, FromJSON)

data Tool =
  Tool
    { tool_id       :: Int
    , name          :: Text
    , description   :: Text
    , lastReturned  :: Day
    , timesBorrowed :: Int
    }

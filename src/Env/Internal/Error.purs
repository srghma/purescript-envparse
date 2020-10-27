module Env.Internal.Error where

import Prelude
import Data.Either
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show
import Data.Maybe
import Data.Unit

data EnvError
  = UnsetError
  | EmptyError
  | UnreadError String

derive instance eqError :: Eq EnvError
derive instance genericError :: Generic EnvError _
instance showError :: Show EnvError where show = genericShow

-- | The class of types that contain and can be constructed from
-- the error returned from parsing unset variables.
class AsUnset e where
  unset :: e
  tryUnset :: e -> Maybe Unit

instance errorAsUnset :: AsUnset EnvError where
  unset = UnsetError
  tryUnset err =
    case err of
      UnsetError -> Just unit
      _ -> Nothing

-- | The class of types that contain and can be constructed from
-- the error returned from parsing variables whose value is empty.
class AsEmpty e where
  empty :: e
  tryEmpty :: e -> Maybe Unit

instance errorAsEmpty :: AsEmpty EnvError where
  empty = EmptyError
  tryEmpty err =
    case err of
      EmptyError -> Just unit
      _ -> Nothing

-- | The class of types that contain and can be constructed from
-- the error returned from parsing variable whose value cannot be parsed.
class AsUnread e where
  unread :: String -> e
  tryUnread :: e -> Maybe String

instance errorAsUnread :: AsUnread EnvError where
  unread = UnreadError
  tryUnread err =
    case err of
      UnreadError msg -> Just msg
      _ -> Nothing

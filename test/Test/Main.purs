module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Env (EnvReader)
import Env as Env
import Data.Maybe
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString

type EnvConfig =
  { name :: String
  , nameOrWorldOnUnset :: String
  , quiet :: Boolean
  , optionalFoo :: Maybe String
  }

nonempty :: forall e . Env.AsEmpty e => EnvReader e String
nonempty = Env.nonEmptyString <#> NonEmptyString.toString

envConfig :: Effect EnvConfig
envConfig = Env.parse (Env.defaultInfo { header = Just "Envparse example" }) ado
  -- unset -> UnsetError
  -- "" -> EmptyError
  -- x -> x
  name <- Env.var nonempty "NAME"
    { help: Just "NAME help"
    , sensitive: true
    , def: Nothing
    , helpDef: Nothing
    }

  -- unset -> "world"
  -- "" -> EmptyError
  -- x -> x
  nameOrWorldOnUnset <- Env.var nonempty "NAME_OR_WORLD_ON_UNSET"
    { help: Just "NAME_OR_WORLD_ON_UNSET help"
    , sensitive: true
    , def: Just "world"
    , helpDef: Nothing
    }

  -- "1" -> true
  -- "true" -> true
  -- _ -> false
  -- unset -> false
  quiet <- Env.switch "QUIET"
    { help: Just "QUIET help"
    , sensitive: false
    }

  -- unset -> Nothing
  -- "" -> EmptyError
  -- x -> Just x
  optionalFoo <- Env.varOptional
    nonempty
    "OPTIONAL_FOO"
    { help: Just "OPTIONAL_FOO help"
    , sensitive: false
    }

  in
    { name
    , nameOrWorldOnUnset
    , quiet
    , optionalFoo
    }

main :: Effect Unit
main = envConfig >>= \x -> log $ "envConfig: " <> show x

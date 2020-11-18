# purescript-envparse

port of https://github.com/supki/envparse with some differences

depends on https://github.com/purescript-node/purescript-node-process/pull/21

Example

```purs
module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Env as Env
import Data.Maybe
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString

type EnvConfig =
  { name :: String
  , quiet :: Boolean
  , optionalFoo :: Maybe NonEmptyString
  }

envConfig :: Effect EnvConfig
envConfig = Env.parse (Env.defaultInfo { header = Just "Envparse example" }) $
  { name: _, quiet: _, optionalFoo: _ }
  <$> Env.var    (Env.nonEmptyString <#> NonEmptyString.toString) "NAME"         (Env.defaultVar { help = Just "Target for the greeting", sensitive = true })
  <*> Env.switch                                                  "QUIET"        (Env.defaultFlag { help = Just "Whether to actually print the greeting ('false' when empty string or unset, true when everything else)" })
  <*> Env.var    (Env.nonEmptyString <#> Just)                    "OPTIONAL_FOO" (Env.defaultVar { help = Just "Some optional ('Nothing' when unset, error when empty string)", def = Just Nothing, helpDef = Just (const "Nothing") })

main :: Effect Unit
main = envConfig >>= \x -> log $ "envConfig: " <> show x
```


```sh
$ ./.spago/run.js
Envparse example

Available environment variables:

  NAME                   Target for the greeting
  OPTIONAL_FOO           Some optional ('Nothing' when
                         unset, error when empty string)
                         (default: Nothing)
  QUIET                  Whether to actually print the
                         greeting ('false' when empty
                         string or unset, true when
                         everything else) (default:
                         false)

Parsing errors:

  NAME is unset

$ NAME="myname" QUIET="" OPTIONAL_FOO="asdf" ./.spago/run.js
envConfig: { name: "myname", optionalFoo: (Just (NonEmptyString.unsafeFromString "asdf")), quiet: false }
```

# TODOs

- the handling of optional (Maybe) vars is not pretty

# Alternatives

- https://github.com/purescript-polyform/batteries-env
- https://github.com/nsaunders/purescript-typedenv

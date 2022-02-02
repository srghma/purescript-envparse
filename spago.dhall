{ name = "envparse"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "either"
  , "lists"
  , "ordered-collections"
  , "strings"
  , "node-process"
  , "transformers"
  , "exists"
  , "boxes"
  , "ansi"
  , "arrays"
  , "bifunctors"
  , "control"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "integers"
  , "maybe"
  , "newtype"
  , "prelude"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

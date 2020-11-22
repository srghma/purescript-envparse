{ name = "envparse"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "nullable"
  , "functions"
  , "either"
  , "generics-rep"
  , "lists"
  , "ordered-collections"
  , "strings"
  , "node-process"
  , "transformers"
  , "exists"
  , "dodo-printer"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

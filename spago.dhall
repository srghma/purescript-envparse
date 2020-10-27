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
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

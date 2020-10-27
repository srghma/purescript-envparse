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
  , "free-alternative"
  , "node-process"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

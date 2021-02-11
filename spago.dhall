{ name = "envparse"
, dependencies =
  [ "console"
  , "effect"
  , "psci-support"
  , "nullable"
  , "functions"
  , "either"
  , "lists"
  , "ordered-collections"
  , "strings"
  , "node-process"
  , "transformers"
  , "exists"
  , "boxes"
  , "ansi"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

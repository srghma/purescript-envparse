{ name = "envparse"
, dependencies = [ "console", "effect", "psci-support", "nullable", "functions" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

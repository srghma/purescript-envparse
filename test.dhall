{ name = "envparse"
  , dependencies = (.spago.dhall).dependencies # [
  - arrays
  - bifunctors
  - control
  - exceptions
  - foldable-traversable
  - foreign-object
  - integers
  - maybe
  - newtype
  - prelude
  - tuples
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220127/packages.dhall sha256:8ccbd53dbc7dbfd92a9cba9cca7a8bf36cb120a0a3e21106bf19a16d3ad6863e

-- let additions = {=}
let additions =
      { dodo-printer =
        { dependencies =
          [ "aff"
          , "ansi"
          , "avar"
          , "console"
          , "effect"
          , "foldable-traversable"
          , "lists"
          , "maybe"
          , "minibench"
          , "node-child-process"
          , "node-fs-aff"
          , "node-process"
          , "psci-support"
          , "strings"
          ]
        , repo = "https://github.com/srghma/purescript-dodo-printer.git"
        , version = "master"
        }
      }

in  upstream // additions

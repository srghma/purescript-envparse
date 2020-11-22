let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201021/packages.dhall sha256:55ebdbda1bd6ede4d5307fbc1ef19988c80271b4225d833c8d6fb9b6fb1aa6d8

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
    , repo = "https://github.com/natefaubion/purescript-dodo-printer.git"
    , version = "master"
    }
  }

in  (upstream // additions)
  with node-process.repo = "https://github.com/srghma/purescript-node-process.git"
  with node-process.version = "master"

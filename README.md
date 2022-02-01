# purescript-envparse

port of https://github.com/supki/envparse with some differences

depends on https://github.com/purescript-node/purescript-node-process/pull/21

Example in `./test/Test/Main.purs`

```sh
$ spago test

Envparse example

Available environment variables:

  NAME                   NAME help
  NAME_OR_WORLD_ON_UNSET
                         NAME_OR_WORLD_ON_UNSET help
  OPTIONAL_FOO           OPTIONAL_FOO help (default:
                         Nothing)
  QUIET                  QUIET help (default: false)

Parsing errors:

  NAME is unset
```

```sh
$ NAME="" spago test

Envparse example

Available environment variables:

  NAME                   NAME help
  NAME_OR_WORLD_ON_UNSET
                         NAME_OR_WORLD_ON_UNSET help
  OPTIONAL_FOO           OPTIONAL_FOO help (default:
                         Nothing)
  QUIET                  QUIET help (default: false)

Parsing errors:

  NAME is empty
```

# TODOs

- [x] the handling of optional (Maybe) vars is not pretty

# Usage examples

https://github.com/srghma/purescript-halogen-nextjs/blob/b9229c8bbeed41a2ad68e74dc08198e0d9bb0d5e/packages/worker/Worker/Config/FromEnv.purs#L8

https://github.com/srghma/purescript-halogen-nextjs/blob/b9229c8bbeed41a2ad68e74dc08198e0d9bb0d5e/packages/worker/Worker/Config.purs#L46

# Alternatives

- https://github.com/purescript-polyform/batteries-env
- https://github.com/nsaunders/purescript-typedenv

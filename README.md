# purescript-envparse

port of https://github.com/supki/envparse with some differences

depends on https://github.com/purescript-node/purescript-node-process/pull/21

Example in `./test/Test/Main.purs`

```sh
$ spago test
$ NAME="" spago test
$ NAME="asdf" spago test
```

# TODOs

- [x] the handling of optional (Maybe) vars is not pretty

# Usage examples

https://github.com/srghma/purescript-halogen-nextjs/blob/b9229c8bbeed41a2ad68e74dc08198e0d9bb0d5e/packages/worker/Worker/Config/FromEnv.purs#L8

https://github.com/srghma/purescript-halogen-nextjs/blob/b9229c8bbeed41a2ad68e74dc08198e0d9bb0d5e/packages/worker/Worker/Config.purs#L46

# Alternatives

- https://github.com/purescript-polyform/batteries-env
- https://github.com/nsaunders/purescript-typedenv

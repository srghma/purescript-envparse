module Env where

parse :: Error.AsUnset e => (Help.Info Error -> Help.Info e) -> Parser e a -> IO a
parse m =
  fmap (either (\_ -> error "absurd") id) . parseOr die m

-- | Try to parse the environment
--
-- Use this if simply dying on failure (the behavior of 'parse') is inadequate for your needs.
parseOr :: Error.AsUnset e => (String -> IO a) -> (Help.Info Error -> Help.Info e) -> Parser e b -> IO (Either a b)
parseOr onFailure helpMod parser = do
  b <- fmap (parsePure parser) getEnvironment
#if __GLASGOW_HASKELL__ >= 708
  for_ b $ \_ ->
    traverseSensitiveVar parser unsetEnv
#endif
  traverseLeft (onFailure . Help.helpInfo (helpMod Help.defaultInfo) parser) b

die :: String -> IO a
die m =
  do IO.hPutStrLn IO.stderr m; exitFailure

traverseLeft :: Applicative f => (a -> f b) -> Either a t -> f (Either b t)
traverseLeft f =
  either (fmap Left . f) (pure . Right)


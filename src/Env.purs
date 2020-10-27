module Env where

import Data.Either
import Effect
import Effect.Exception.Unsafe
import Env.Internal.Error
import Env.Internal.Parser
import Prelude

import Data.Foldable (for_)
import Effect.Console (error) as Console
import Env.Internal.Error as Error
import Env.Internal.Help as Help
import Node.Process as NodeProcess

parse :: forall e a . Error.AsUnset e => (Help.Info EnvError -> Help.Info e) -> Parser e a -> Effect a
parse m =
  map (either (\_ -> unsafeThrow "absurd") identity) <<< parseOr die m

-- | Try to parse the environment
--
-- Use this if simply dying on failure (the behavior of 'parse') is inadequate for your needs.
parseOr :: forall e a b . Error.AsUnset e => (String -> Effect a) -> (Help.Info EnvError -> Help.Info e) -> Parser e b -> Effect (Either a b)
parseOr onFailure helpMod parser = do
  b <- map (parsePure parser) NodeProcess.getEnv
  for_ b $ \_ ->
    traverseSensitiveVar parser NodeProcess.unsetEnv
  traverseLeft (onFailure <<< Help.helpInfo (helpMod Help.defaultInfo) parser) b

die :: forall a . String -> Effect a
die m = do
  Console.error m
  NodeProcess.exit 1

traverseLeft :: forall f a b t . Applicative f => (a -> f b) -> Either a t -> f (Either b t)
traverseLeft f =
  either (map Left <<< f) (pure <<< Right)


module Env
  ( module Env
  , module Export
  ) where

import Data.Either (Either(..), either)
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Env.Internal.Parser (Parser, parsePure, traverseSensitiveVar)
import Prelude

import Data.Foldable (for_)
import Effect.Console (error) as Console
import Env.Internal.Error as Error
import Env.Internal.Help as Help
import Node.Process as NodeProcess
import Text.PrettyPrint.Boxes as Boxes

import Env.Internal.Error (class AsEmpty, class AsUnread, class AsUnset, EnvError(..), empty, tryEmpty, tryUnread, tryUnset, unread, unset) as Export
import Env.Internal.Parser (DefaultVar, EnvReader, OptionalVarOptions, Parser(..), SwitchOptions, VarF(..), VarOptions, addName, char, defaultOptionalVarOptions, defaultSwitchOptions, defaultVar, defaultVarOptions, int, liftVarF, lookupVar, nonEmptyString, optionalVar, parsePure, prefixed, readVar, sensitive, split, str, switch, traverseSensitiveVar, var, nonempty) as Export
import Env.Internal.Help (ErrorHandler, HelpParserDocInfo, HelpParserDocInfo', Info, defaultErrorHandler, defaultInfo, handleEmptyError, handleUnreadError, handleUnsetError, helpInfo, helpParserDoc, styleName, styleVal) as Export

parse :: forall e a . Error.AsUnset e => Help.Info e -> Parser e a -> Effect a
parse m =
  map (either (\_ -> unsafeThrow "absurd") identity) <<< parseOr die m

-- | Try to parse the environment
--
-- Use this if simply dying on failure (the behavior of 'parse') is inadequate for your needs.
parseOr :: forall e a b . Error.AsUnset e => (String -> Effect a) -> Help.Info e -> Parser e b -> Effect (Either a b)
parseOr onFailure info parser = do
  b <- map (parsePure parser) NodeProcess.getEnv
  for_ b $ \_ ->
    traverseSensitiveVar parser NodeProcess.unsetEnv
  traverseLeft (onFailure <<< Boxes.render <<< Help.helpInfo info parser) b

die :: forall a . String -> Effect a
die m = do
  Console.error m
  NodeProcess.exit 1

traverseLeft :: forall f a b t . Applicative f => (a -> f b) -> Either a t -> f (Either b t)
traverseLeft f =
  either (map Left <<< f) (pure <<< Right)


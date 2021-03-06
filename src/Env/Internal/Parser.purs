module Env.Internal.Parser where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus, alt, empty)
import Control.Monad.Reader.Trans (ReaderT(..), runReaderT)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Foldable (traverse_)
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (toChar) as String
import Data.String.Common (split) as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Data.String.Pattern (Pattern)
import Data.Tuple (Tuple(..))
import Env.Internal.Error as Error
import Env.Internal.Free (Alt)
import Env.Internal.Free as Free
import Env.Internal.Val (Val)
import Env.Internal.Val as Val
import Foreign.Object (Object)
import Foreign.Object as Object

-- | Try to parse a pure environment
parsePure :: forall e a . Error.AsUnset e => Parser e a -> Object String -> Either (Array (Tuple String e)) a
parsePure (Parser p) env =
  Val.toEither (Free.runAlt go' p)
  where
    go :: forall a' . VarF e a' -> Either (Tuple String e) a'
    go (VarF varF) =
      case lookupVar (VarF varF) env of
        Left lookupErr ->
          case varF.default of
               Nothing -> Left lookupErr
               Just { value } -> Right value
        Right val ->
          readVar (VarF varF) val

    go' :: forall a' . VarF e a' -> Val (Array (Tuple String e)) a'
    go' = Val.fromEither <<< lmap Array.singleton <<< go

traverseSensitiveVar :: forall m e a b . Applicative m => Parser e a -> (String -> m b) -> m Unit
traverseSensitiveVar (Parser parser) f =
  traverse_ f sensitiveVars
 where
    sensitiveVars :: Set String
    sensitiveVars =
      Free.foldAlt (\(VarF varF) -> if varF.sensitive then Set.singleton varF.name else Set.empty) parser

readVar :: forall e a . VarF e a -> String -> Either (Tuple String e) a
readVar (VarF varF) =
  addName varF.name <<< runReaderT varF.reader

lookupVar :: forall e a . Error.AsUnset e => VarF e a -> Object String -> Either (Tuple String e) String
lookupVar (VarF varF) =
  addName varF.name <<< maybe (Left Error.unset) Right <<< Object.lookup varF.name

addName :: forall e a . String -> Either e a -> Either (Tuple String e) a
addName name =
  lmap (Tuple name)

-- | An environment parser
newtype Parser e a = Parser (Alt (VarF e) a)

derive instance newtypeParser :: Newtype (Parser e a) _
derive instance functorParser :: Functor (Parser e)

instance applyParser :: Apply (Parser e) where
  apply (Parser f) (Parser x) = Parser (apply f x)

instance applicativeParser :: Applicative (Parser e) where
  pure = Parser <<< pure

instance altParser :: Alt (Parser e) where
  alt (Parser f) (Parser x) = Parser (alt f x)

instance plusParser :: Monoid e => Plus (Parser e) where
  empty = Parser empty

instance alternativeParser :: Monoid e => Alternative (Parser e)

-- | The string to prepend to the name of every declared environment variable
prefixed :: forall e a . String -> Parser e a -> Parser e a
prefixed pre =
  Parser <<< Free.hoistAlt (\(VarF varF) -> VarF (varF { name = pre <> varF.name })) <<< unwrap

-- | Mark the enclosed variables as sensitive to remove them from the environment
-- once they've been parsed successfully.
sensitive :: forall e a . Parser e a -> Parser e a
sensitive =
  Parser <<< Free.hoistAlt (\(VarF varF) -> VarF (varF { sensitive = true })) <<< unwrap

type DefaultVar a = { value :: a, help :: Maybe String }

newtype VarF e a = VarF
  { name      :: String
  , reader    :: EnvReader e a
  , help      :: Maybe String
  , default   :: Maybe (DefaultVar a)
  , sensitive :: Boolean
  }

derive instance functorVarF :: Functor (VarF e)

liftVarF :: forall e a . VarF e a -> Parser e a
liftVarF =
  Parser <<< Free.liftAlt

-- | An environment variable's value parser. Use @(<=<)@ and @(>=>)@ to combine these
type EnvReader e a = ReaderT String (Either e) a

defaultVar :: forall a . Show a => a -> DefaultVar a
defaultVar value = { value, help: Just (show value) }

type VarOptions a =
  { help      :: Maybe String
  , default   :: Maybe (DefaultVar a)
  , sensitive :: Boolean
  }

defaultVarOptions :: forall a . VarOptions a
defaultVarOptions =
  { help:      Nothing
  , default:   Nothing
  , sensitive: false
  }

-- | Parse a particular variable from the environment
var
  :: forall e a
   . Error.AsUnset e
  => EnvReader e a
  -> String
  -> VarOptions a
  -> Parser e a
var reader name varConfig =
  liftVarF $ VarF
    { name
    , reader
    , help: varConfig.help
    , default: varConfig.default
    , sensitive: varConfig.sensitive
    }

type OptionalVarOptions =
  { help      :: Maybe String
  , sensitive :: Boolean
  }

defaultOptionalVarOptions :: OptionalVarOptions
defaultOptionalVarOptions =
  { help:      Nothing
  , sensitive: false
  }

-- | Parse a particular variable from the environment
optionalVar
  :: forall e a
   . Error.AsUnset e
  => EnvReader e a
  -> String
  -> OptionalVarOptions
  -> Parser e (Maybe a)
optionalVar reader name varConfig =
  liftVarF $ VarF
    { name
    , reader: reader <#> Just
    , help: varConfig.help
    , default: Just { value: Nothing, help: Just "Nothing" }
    , sensitive: varConfig.sensitive
    }

type SwitchOptions =
  { help      :: Maybe String
  , sensitive :: Boolean
  }

defaultSwitchOptions :: SwitchOptions
defaultSwitchOptions =
  { help:      Nothing
  , sensitive: false
  }

-- | A simple boolean 'flag'
-- |
-- | "1" -> true
-- | "true" -> true
-- | _ -> false
-- | unset -> false
--
-- /Note:/ this parser never fails.
switch
  :: forall e
   . String
  -> SwitchOptions
  -> Parser e Boolean
switch name config =
  liftVarF $ VarF
    { name
    , reader: ReaderT $ Right <<<
      case _ of
           "1"    -> true
           "true" -> true
           _      -> false
    , help: config.help
    , default: Just (defaultVar false)
    , sensitive: config.sensitive
    }

nonempty :: forall e . Error.AsEmpty e => EnvReader e String
nonempty = nonEmptyString <#> NonEmptyString.toString

-- | The trivial reader
str :: forall e . EnvReader e String
str = ReaderT Right

-- | The reader that accepts only non-empty strings
nonEmptyString :: forall e . Error.AsEmpty e => EnvReader e NonEmptyString
nonEmptyString = ReaderT $ NonEmptyString.fromString >>> note Error.empty

-- | The single character string reader
char :: forall e . Error.AsUnread e => EnvReader e Char
char = ReaderT $ \s -> String.toChar s # note (Error.unread "must be a one-character string")

int :: forall e . Error.AsUnread e => EnvReader e Int
int = ReaderT $ \s -> Int.fromString s # note (Error.unread "must be an integer")

-- TODO
-- | number :: forall e . Error.AsUnread e => EnvReader e Number
-- | number = ReaderT $ \s -> Number.fromString s # note (Error.unread "must be a number")

-- | The reader that splits a string into a list of strings consuming the separator.
split :: forall e . Pattern -> EnvReader e (Array String)
split sep = ReaderT $ Right <<< String.split sep

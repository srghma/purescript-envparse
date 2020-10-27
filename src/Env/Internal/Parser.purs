module Env.Internal.Parser where

import Control.Alternative (class Alt, class Alternative, class Plus, alt, empty)
import Control.Alternative.Free (FreeAlternative, foldFreeAlternative, hoistFreeAlternative, liftFreeAlternative)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Foldable (traverse_)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (toChar) as String
import Data.String.Common (split) as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Data.String.Pattern (Pattern)
import Env.Internal.Error (EnvError)
import Env.Internal.Error as Error
import Env.Internal.Val (Val)
import Env.Internal.Val as Val
import Foreign.Object (Object)
import Foreign.Object as Object
import Env.Internal.Free as Free

-- | Try to parse a pure environment
parsePure :: forall e a . Error.AsUnset e => Parser e a -> Object String -> Either (Array (Tuple String e)) a
parsePure (Parser p) env =
  Val.toEither (foldFreeAlternative go' p)
  where
    go :: forall a' . VarF e a' -> Either (Tuple String e) a'
    go (VarF varF) =
      case lookupVar (VarF varF) env of
        Left lookupErr ->
          maybe (Left lookupErr) Right varF.def
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
      Free.foldMonoidFreeAlternative (\(VarF varF) -> if varF.sensitive then Set.singleton varF.name else Set.empty) parser

readVar :: forall e a . VarF e a -> String -> Either (Tuple String e) a
readVar (VarF varF) =
  addName varF.name <<< varF.reader

lookupVar :: forall e a . Error.AsUnset e => VarF e a -> Object String -> Either (Tuple String e) String
lookupVar (VarF varF) =
  addName varF.name <<< maybe (Left Error.unset) Right <<< Object.lookup varF.name

addName :: forall e a . String -> Either e a -> Either (Tuple String e) a
addName name =
  lmap (Tuple name)

-- | An environment parser
newtype Parser e a = Parser (FreeAlternative (VarF e) a)

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
  Parser <<< hoistFreeAlternative (\(VarF varF) -> VarF (varF { name = pre <> varF.name })) <<< unwrap

-- | Mark the enclosed variables as sensitive to remove them from the environment
-- once they've been parsed successfully.
sensitive :: forall e a . Parser e a -> Parser e a
sensitive =
  Parser <<< hoistFreeAlternative (\(VarF varF) -> VarF (varF { sensitive = true })) <<< unwrap

newtype VarF e a = VarF
  { name      :: String
  , reader    :: Reader e a
  , help      :: Maybe String
  , def       :: Maybe a
  , helpDef   :: Maybe String
  , sensitive :: Boolean
  }

derive instance functorVarF :: Functor (VarF e)

liftVarF :: forall e a . VarF e a -> Parser e a
liftVarF =
  Parser <<< liftFreeAlternative

-- | An environment variable's value parser. Use @(<=<)@ and @(>=>)@ to combine these
type Reader e a = String -> Either e a

-- | Parse a particular variable from the environment
--
-- @
-- >>> var 'str' \"EDITOR\" ('def' \"vim\" <> 'helpDef' show)
-- @
var :: forall e a . Error.AsUnset e => Reader e a -> String -> Var a -> Parser e a
var r n varConfig =
  liftVarF $ VarF
    { name: n
    , reader: r
    , help: varConfig.help
    , def: varConfig.def
    , helpDef: varConfig.helpDef <*> varConfig.def
    , sensitive: varConfig.sensitive
    }

-- | A flag that takes the active value if the environment variable
-- is set and non-empty and the default value otherwise
--
-- /Note:/ this parser never fails.
flag
  :: forall e a
   . a -- ^ default value
  -> a -- ^ active value
  -> String -> Flag a -> Parser e a
flag f t n flagConfig =
  liftVarF $ VarF
    { name: n
    , reader: \val ->
        pure $ case (nonEmptyString :: Reader EnvError NonEmptyString) val of
          Left  _ -> f
          Right _ -> t
    , help: flagConfig.help
    , def: Just f
    , helpDef: Nothing
    , sensitive: flagConfig.sensitive
    }

-- | A simple boolean 'flag'
--
-- /Note:/ this parser never fails.
switch :: forall e . String -> Flag Boolean -> Parser e Boolean
switch =
  flag false true

-- | The trivial reader
str :: forall e . Reader e String
str = Right

-- | The reader that accepts only non-empty strings
nonEmptyString :: forall e . Error.AsEmpty e => Reader e NonEmptyString
nonEmptyString = NonEmptyString.fromString >>> note Error.empty

-- | The single character string reader
char :: forall e . Error.AsUnread e => Reader e Char
char s = String.toChar s # note (Error.unread "must be a one-character string")

-- | The reader that splits a string into a list of strings consuming the separator.
split :: forall e . Pattern -> Reader e (Array String)
split sep = Right <<< String.split sep

-- | Environment variable metadata
type Var a =
  { help      :: Maybe String
  , helpDef   :: Maybe (a -> String)
  , def       :: Maybe a
  , sensitive :: Boolean
  }

defaultVar :: forall a . Var a
defaultVar =
  { help: Nothing
  , def: Nothing
  , helpDef: Nothing
  , sensitive: defaultSensitive
  }

defaultSensitive :: Boolean
defaultSensitive = false

-- | Flag metadata
type Flag a =
  { help      :: Maybe String
  , sensitive :: Boolean
  }

defaultFlag :: forall a . Flag a
defaultFlag =
  { help: Nothing
  , sensitive: defaultSensitive
  }

module Env.Internal.Parser where

import Control.Alt
import Control.Alternative
import Control.Alternative.Free
import Data.Either
import Data.Maybe
import Data.Tuple
import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Foldable (fold, foldMap, traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits as String
import Data.String.Common as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString
import Data.String.Pattern (Pattern(..))
import Data.String.Pattern as String
import Env.Internal.Error (EnvError)
import Env.Internal.Error as Error
import Env.Internal.Val (Val)
import Env.Internal.Val as Val
import Foreign.Object (Object)
import Foreign.Object as Object
import Unsafe.Coerce (unsafeCoerce)
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
          maybe (Left lookupErr) Right varF.varfDef
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
      Free.foldMonoidFreeAlternative (\(VarF varF) -> if varF.varfSensitive then Set.singleton varF.varfName else Set.empty) parser

readVar :: forall e a . VarF e a -> String -> Either (Tuple String e) a
readVar (VarF varF) =
  addName varF.varfName <<< varF.varfReader

lookupVar :: forall e a . Error.AsUnset e => VarF e a -> Object String -> Either (Tuple String e) String
lookupVar (VarF varF) =
  addName varF.varfName <<< maybe (Left Error.unset) Right <<< Object.lookup varF.varfName

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
  Parser <<< hoistFreeAlternative (\(VarF varF) -> VarF (varF { varfName = pre <> varF.varfName })) <<< unwrap

-- | Mark the enclosed variables as sensitive to remove them from the environment
-- once they've been parsed successfully.
sensitive :: forall e a . Parser e a -> Parser e a
sensitive =
  Parser <<< hoistFreeAlternative (\(VarF varF) -> VarF (varF { varfSensitive = true })) <<< unwrap

newtype VarF e a = VarF
  { varfName      :: String
  , varfReader    :: Reader e a
  , varfHelp      :: Maybe String
  , varfDef       :: Maybe a
  , varfHelpDef   :: Maybe String
  , varfSensitive :: Boolean
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
var :: forall e a . Error.AsUnset e => Reader e a -> String -> (Var a -> Var a) -> Parser e a
var r n mod =
  liftVarF $ VarF
    { varfName: n
    , varfReader: r
    , varfHelp: varHelp
    , varfDef: varDef
    , varfHelpDef: varHelpDef <*> varDef
    , varfSensitive: varSensitive
    }
 where
  { varHelp, varDef, varHelpDef, varSensitive } = mod defaultVar

-- | A flag that takes the active value if the environment variable
-- is set and non-empty and the default value otherwise
--
-- /Note:/ this parser never fails.
flag
  :: forall e a
   . a -- ^ default value
  -> a -- ^ active value
  -> String -> (Flag a -> Flag a) -> Parser e a
flag f t n mod =
  liftVarF $ VarF
    { varfName: n
    , varfReader: \val ->
        pure $ case (nonempty :: Reader EnvError NonEmptyString) val of
          Left  _ -> f
          Right _ -> t
    , varfHelp: flagHelp
    , varfDef: Just f
    , varfHelpDef: Nothing
    , varfSensitive: flagSensitive
    }
 where
  { flagHelp, flagSensitive } = mod defaultFlag

-- | A simple boolean 'flag'
--
-- /Note:/ this parser never fails.
switch :: forall e . String -> (Flag Boolean -> Flag Boolean) -> Parser e Boolean
switch =
  flag false true

-- | The trivial reader
str :: forall e . Reader e String
str = Right

-- | The reader that accepts only non-empty strings
nonempty :: forall e . Error.AsEmpty e => Reader e NonEmptyString
nonempty = NonEmptyString.fromString >>> note Error.empty

-- | The single character string reader
char :: forall e . Error.AsUnread e => Reader e Char
char s = String.toChar s # note (Error.unread "must be a one-character string")

-- | The reader that splits a string into a list of strings consuming the separator.
split :: forall e . Pattern -> Reader e (Array String)
split sep = Right <<< String.split sep

-- | Environment variable metadata
type Var a =
  { varHelp      :: Maybe String
  , varHelpDef   :: Maybe (a -> String)
  , varDef       :: Maybe a
  , varSensitive :: Boolean
  }

defaultVar :: forall a . Var a
defaultVar =
  { varHelp: Nothing
  , varDef: Nothing
  , varHelpDef: Nothing
  , varSensitive: defaultSensitive
  }

defaultSensitive :: Boolean
defaultSensitive = false

-- | Flag metadata
type Flag a =
  { flagHelp      :: Maybe String
  , flagSensitive :: Boolean
  }

defaultFlag :: forall a . Flag a
defaultFlag =
  { flagHelp: Nothing
  , flagSensitive: defaultSensitive
  }

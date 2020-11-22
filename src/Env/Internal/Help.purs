module Env.Internal.Help where

import Prelude

import Data.Array (foldr)
import Data.Array as Array
import Data.Foldable (oneOf)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..), uncurry)
import Dodo (Doc)
import Dodo as Dodo
import Dodo.Internal as Dodo
import Dodo.Ansi (GraphicsParam)
import Dodo.Ansi as Dodo.Ansi
import Env.Internal.Error (EnvError)
import Env.Internal.Error as Error
import Env.Internal.Free as Free
import Env.Internal.Parser (Parser, VarF(..))

appendDoubleBreak :: forall a. Doc a -> Doc a -> Doc a
appendDoubleBreak = Dodo.bothNotEmpty \a b -> a <> (Dodo.break <> Dodo.break <> b)

doubleLines = foldr appendDoubleBreak Dodo.Empty

helpInfo :: forall e a . Info e -> Parser e a -> Array (Tuple String e) -> Doc GraphicsParam
helpInfo info parser errors =
  doubleLines $ Array.catMaybes
    [ map Dodo.text info.header
    , map Dodo.text info.description
    , Just $ (Dodo.text "Available environment variables:") `appendDoubleBreak` (helpParserDoc parser)
    -- | , map (String.joinWith "\n" <<< splitWords 50) info.footer
    ]
    -- | <> helpErrors info.handleError errors

helpParserDoc :: forall e a . Parser e a -> Doc GraphicsParam
helpParserDoc =
  Dodo.lines <<< Array.fromFoldable <<< Map.values <<< Free.foldAlt go <<< unwrap
  where
    go :: forall a' . VarF e a' -> Map String (Doc GraphicsParam)
    go (VarF v) = Map.singleton v.name (helpVarfDoc (VarF v))

helpVarfDoc :: forall e a . VarF e a -> Doc GraphicsParam
helpVarfDoc (VarF varF) =
  let
    name :: Doc GraphicsParam
    name = Dodo.text varF.name

    defaultValueHelp :: Doc GraphicsParam
    defaultValueHelp =
      case varF.default of
           Nothing -> Dodo.Empty
           Just defaultConfig ->
             case defaultConfig.help of
                  Nothing -> Dodo.Empty
                  Just defaultConfigHelp -> Dodo.text $ "(default: " <> defaultConfigHelp <>")"

    help :: Doc GraphicsParam
    help =
      case varF.help of
        Nothing -> Dodo.Empty
        Just help -> Dodo.text help

  in Dodo.indent $ name `Dodo.appendSpaceBreak` (Dodo.alignCurrentColumn $ help `Dodo.appendSpaceBreak` defaultValueHelp)

-- | helpErrors :: forall e . ErrorHandler e -> Array (Tuple String e) -> Doc GraphicsParam
-- | helpErrors _       [] = Dodo.Empty
-- | helpErrors handler fs = Dodo.lines $
-- |   [ Dodo.text "Parsing errors:"
-- |   ] <> Array.mapMaybe (uncurry handler) $ Array.sortBy (comparing (\(Tuple varName _) -> varName) fs)

-- | Parser's metadata
type Info e =
  { header      :: Maybe String
  , description :: Maybe String
  , footer      :: Maybe String
  , handleError :: ErrorHandler e
  }

-- | Given a variable name and an error value, try to produce a useful error message
type ErrorHandler e = String -> e -> Maybe (Doc GraphicsParam)

defaultInfo :: Info EnvError
defaultInfo =
  { header:      Nothing
  , description: Nothing
  , footer:      Nothing
  , handleError: defaultErrorHandler
  }

-- | The default error handler
defaultErrorHandler :: forall e . Error.AsUnset e => Error.AsEmpty e => Error.AsUnread e => ErrorHandler e
defaultErrorHandler name err =
  oneOf
    [ handleUnsetError name err
    , handleEmptyError name err
    , handleUnreadError name err
    ]

handleUnsetError :: forall e . Error.AsUnset e => ErrorHandler e
handleUnsetError name =
  map (\_ -> Dodo.indent $ Dodo.text $ name <> " is unset") <<< Error.tryUnset

handleEmptyError :: forall e . Error.AsEmpty e => ErrorHandler e
handleEmptyError name =
  map (\_ -> Dodo.indent $ Dodo.text $ name <> " is empty") <<< Error.tryEmpty

handleUnreadError :: forall e . Error.AsUnread e => ErrorHandler e
handleUnreadError name =
  map (\val -> Dodo.indent $ Dodo.text $ name <> " has value " <> val <> " that cannot be parsed") <<< Error.tryUnread

module Env.Internal.Help where

import Prelude

import Data.Array as Array
import Data.Foldable (oneOf, foldr)
import Data.List (List(..))
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Env.Internal.Error (EnvError)
import Env.Internal.Error as Error
import Env.Internal.Free as Free
import Env.Internal.Parser (Parser, VarF(..))
import Dodo (Doc, (<%>))
import Dodo as Dodo
import Ansi.Codes (Color(..)) as Ansi
import Ansi.Output (bold, foreground, withGraphics) as Ansi

helpInfo :: forall e a . Info e -> Parser e a -> Array (Tuple String e) -> Doc Void
helpInfo info parser errors = Dodo.foldWithSeparator (Dodo.break <> Dodo.break) $ Array.catMaybes
  [ map Dodo.textParagraph info.header
  , map Dodo.textParagraph info.description
  , Just $
      Dodo.text "Available environment variables:" <%>
      helpParserDoc parser
  , map Dodo.textParagraph info.footer
  , helpErrors info.handleError errors
  ]
  where
    helpErrors :: ErrorHandler e -> Array (Tuple String e) -> Maybe (Doc Void)
    helpErrors _       [] = Nothing
    helpErrors handler fs =
      Just $ Dodo.text "Parsing errors:" <%>
          ( Dodo.indent
          $ Dodo.foldWithSeparator Dodo.break
          $ Array.mapMaybe (\(Tuple name error) -> map Dodo.textParagraph (handler name error))
          $ Array.sortBy (comparing (\(Tuple varName _) -> varName)) fs
          )

type HelpParserDocInfo =
  { name :: String
  , help :: Maybe String
  , defaultValueHelp :: Maybe String
  }

helpParserDoc :: forall e a . Parser e a -> Doc Void
helpParserDoc =
  ?a
  <<< map (\config ->
    Dodo.indent $ Dodo.flexGroup $ Dodo.paragraph $ Array.catMaybes
    [ Maybe (Dodo.textParagraph <<< styleName) config.name
    , map (Dodo.textParagraph) config.help
    , map (Dodo.textParagraph) config.defaultValueHelp
    ]
  )
  <<< Map.values
  <<< unwrap
  <<< Free.foldAlt collectInfoUniq
  <<< unwrap
  where
    collectInfo
      :: forall e a
       . VarF e a
      -> HelpParserDocInfo
    collectInfo (VarF varF) =
      { name: varF.name
      , help: varF.help
      , defaultValueHelp:
        varF.default
        >>= _.help
        >>= \help -> Just $ "(default: " <> styleVal help <>")"
      }

    collectInfoUniq
      :: forall a'
       . VarF e a'
      -> SemigroupMap String HelpParserDocInfo
    collectInfoUniq (VarF v) = SemigroupMap $ Map.singleton v.name (collectInfo (VarF v))

-- | Parser's metadata
type Info e =
  { header      :: Maybe String
  , description :: Maybe String
  , footer      :: Maybe String
  , handleError :: ErrorHandler e
  }

-- | Given a variable name and an error value, try to produce a useful error message
type ErrorHandler e = String -> e -> Maybe String

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
  map (\_ -> styleName name <> " is unset") <<< Error.tryUnset

handleEmptyError :: forall e . Error.AsEmpty e => ErrorHandler e
handleEmptyError name =
  map (\_ -> styleName name <> " is empty") <<< Error.tryEmpty

handleUnreadError :: forall e . Error.AsUnread e => ErrorHandler e
handleUnreadError name =
  map (\val -> styleName name <> " has value " <> styleVal val <> " that cannot be parsed") <<< Error.tryUnread

styleName :: String → String
styleName = identity -- Ansi.withGraphics (Ansi.bold <> Ansi.foreground Ansi.Red)

styleVal :: String → String
styleVal = identity -- Ansi.withGraphics (Ansi.bold <> Ansi.foreground Ansi.Green)

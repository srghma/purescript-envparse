module Env.Internal.Help where

import Prelude

import Data.Array (foldr)
import Data.Array as Array
import Data.Foldable (oneOf)
import Data.List (List(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Env.Internal.Error (EnvError)
import Env.Internal.Error as Error
import Env.Internal.Free as Free
import Env.Internal.Parser (Parser, VarF(..))
import Text.PrettyPrint.Boxes (Box)
import Text.PrettyPrint.Boxes as Boxes
import Ansi.Codes (Color(..)) as Ansi
import Ansi.Output (bold, foreground, withGraphics) as Ansi

helpInfo :: forall e a . Info e -> Parser e a -> Array (Tuple String e) -> Box
helpInfo = \info parser errors ->
  Boxes.punctuateV Boxes.left (Boxes.emptyBox 1 0) $ Array.catMaybes
    [ map (Boxes.para Boxes.left 80) info.header
    , map (Boxes.para Boxes.left 80) info.description
    , Just $
        (Boxes.text "Available environment variables:")
        Boxes.//
        (helpParserDoc parser)
    , map (Boxes.para Boxes.left 80) info.footer
    , helpErrors info.handleError errors
    ]
  where
    helpErrors :: forall e . ErrorHandler e -> Array (Tuple String e) -> Maybe Box
    helpErrors _       [] = Nothing
    helpErrors handler fs =
      Just $
        (Boxes.text "Parsing errors:")
        Boxes.//
        ( Boxes.moveRight 2
        $ Boxes.hcat Boxes.left
        $ Array.mapMaybe (\(Tuple name error) -> map (Boxes.para Boxes.left 80)
        $ handler name error)
        $ Array.sortBy (comparing (\(Tuple varName _) -> varName)) fs
        )

type HelpParserDocInfo =
  { name :: String
  , help :: Maybe String
  , defaultValueHelp :: Maybe String
  }

type HelpParserDocInfo' =
  { names             :: List String
  , helps             :: List (Maybe String)
  , defaultValueHelps :: List (Maybe String)
  }

helpParserDoc :: forall e a . Parser e a -> Box
helpParserDoc =
  (\config ->
    Boxes.moveRight 2 $ Boxes.hsep 1 Boxes.left
    [ Boxes.vcat Boxes.left $ map (Boxes.para Boxes.left 80 <<< styleName) config.names
    , Boxes.vcat Boxes.left $ map (maybe Boxes.nullBox (Boxes.para Boxes.left 40)) config.helps
    , Boxes.vcat Boxes.left $ map (maybe Boxes.nullBox (Boxes.para Boxes.left 40)) config.defaultValueHelps
    ]
  )
  <<< foldr collectInfos
      { names: Nil
      , helps: Nil
      , defaultValueHelps: Nil
      }
  -- | Boxes.moveRight 2
  -- | <<< Boxes.vcat Boxes.left
  -- | <<< map
  -- | (\config ->
  -- |   Boxes.hsep 1 Boxes.left
  -- |   [ Boxes.para Boxes.left 80 $ styleName config.name
  -- |   , maybe Boxes.nullBox (Boxes.para Boxes.left 40) config.help
  -- |   , maybe Boxes.nullBox (Boxes.para Boxes.left 40) config.defaultValueHelp
  -- |   ]
  -- | )
  <<< Map.values
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
      -> Map String HelpParserDocInfo
    collectInfoUniq (VarF v) = Map.singleton v.name (collectInfo (VarF v))

    collectInfos :: HelpParserDocInfo -> HelpParserDocInfo' -> HelpParserDocInfo'
    collectInfos info infoAcc =
      { names:             Cons info.name infoAcc.names
      , helps:             Cons info.help infoAcc.helps
      , defaultValueHelps: Cons info.defaultValueHelp infoAcc.defaultValueHelps
      }

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
styleName = Ansi.withGraphics (Ansi.bold <> Ansi.foreground Ansi.Red)

styleVal :: String → String
styleVal = Ansi.withGraphics (Ansi.bold <> Ansi.foreground Ansi.Green)

module Env.Internal.Help where

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)
import Env.Internal.Error (EnvError)
import Env.Internal.Parser (Parser, VarF(..))
import Prelude

import Data.Array as Array
import Data.Foldable (oneOf)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Env.Internal.Error as Error
import Env.Internal.Free as Free

helpInfo :: forall e a . Info e -> Parser e a -> Array (Tuple String e) -> String
helpInfo info p errors =
  String.joinWith "\n\n" $ Array.catMaybes
    [ info.header
    , map (String.joinWith "\n" <<< splitWords 50) info.desc
    , Just $ String.joinWith "\n" $ Array.cons "Available environment variables:\n" (helpParserDoc p)
    , map (String.joinWith "\n" <<< splitWords 50) info.footer
    ] <> helpErrors info.handleError errors

helpParserDoc :: forall e a . Parser e a -> Array String
helpParserDoc =
  join <<< Array.fromFoldable <<< Map.values <<< Free.foldAlt go <<< unwrap
  where
    go :: forall a' . VarF e a' -> Map String (Array String)
    go (VarF v) = Map.singleton v.name (helpVarfDoc (VarF v))

helpVarfDoc :: forall e a . VarF e a -> Array String
helpVarfDoc (VarF varF) =
  case varF.help of
    Nothing -> [indent 2 varF.name]
    Just h ->
      let
          t =
            case varF.default of
                 Nothing -> h
                 Just { help } ->
                   case help of
                        Nothing -> h
                        Just s -> h <> " (default: " <> s <>")"
      in
        case unit of
          _ | k > 15    -> Array.cons (indent 2 varF.name) (map (indent 25) (splitWords 30 t))
            | otherwise ->
                let
                    indentParagraph :: Array String -> Array String
                    indentParagraph =
                      mapWithIndex \i v ->
                        if i == 0
                          then indent (23 - k) v
                          else indent 25 v

                in case Array.uncons $ indentParagraph (splitWords 30 t) of
                        Just { head, tail } -> Array.cons ((indent 2 varF.name) <> head) tail
                        _ -> [indent 2 varF.name]
  where
  k = String.length varF.name

splitWords :: Int -> String -> Array String
splitWords n =
  Array.fromFoldable <<< go Nil 0 <<< List.fromFoldable <<< String.split (Pattern " ")
  where
  go :: List String -> Int -> List String -> List String
  go acc _ Nil = prep acc
  go acc k (w : ws) =
    let
      z = String.length w
    in case unit of
            _ | k + z < n -> go (w : acc) (k + z) ws
              | z > n     ->
                prep acc <>
                case String.splitAt n w of
                    { before, after } -> before : go Nil 0 (after : ws)
              | otherwise -> prep acc <> go (List.singleton w) z ws

  prep :: List String -> List String
  prep Nil  = Nil
  prep acc = List.singleton $ String.joinWith " " $ List.toUnfoldable $ List.reverse acc

indent :: Int -> String -> String
indent n s =
  Array.fold (Array.replicate n " ") <> s

helpErrors :: forall e . ErrorHandler e -> Array (Tuple String e) -> Array String
helpErrors _       [] = []
helpErrors handler fs =
  [ "Parsing errors:"
  , String.joinWith "\n" (Array.mapMaybe (uncurry handler) (Array.sortBy (comparing (\(Tuple varName _) -> varName)) fs))
  ]

-- | Parser's metadata
type Info e =
  { header      :: Maybe String
  , desc        :: Maybe String
  , footer      :: Maybe String
  , handleError :: ErrorHandler e
  }

-- | Given a variable name and an error value, try to produce a useful error message
type ErrorHandler e = String -> e -> Maybe String

defaultInfo :: Info EnvError
defaultInfo =
  { header: Nothing
  , desc: Nothing
  , footer: Nothing
  , handleError: defaultErrorHandler
  }

-- | The default error handler
defaultErrorHandler :: forall e . Error.AsUnset e => Error.AsEmpty e => Error.AsUnread e => ErrorHandler e
defaultErrorHandler name err =
  oneOf [handleUnsetError name err, handleEmptyError name err, handleUnreadError name err]

handleUnsetError :: forall e . Error.AsUnset e => ErrorHandler e
handleUnsetError name =
  map (\_ -> indent 2 (name <> " is unset")) <<< Error.tryUnset

handleEmptyError :: forall e . Error.AsEmpty e => ErrorHandler e
handleEmptyError name =
  map (\_ -> indent 2 (name <> " is empty")) <<< Error.tryEmpty

handleUnreadError :: forall e . Error.AsUnread e => ErrorHandler e
handleUnreadError name =
  map (\val -> indent 2 (name <> " has value " <> val <> " that cannot be parsed")) <<< Error.tryUnread

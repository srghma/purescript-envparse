module Env.Internal.Help where

import Data.Either
import Data.Generic.Rep.Show
import Env.Internal.Error
import Prelude

import Data.Foldable (oneOf)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.Maybe

helpInfo :: Info e -> Parser e b -> Array (Tuple String e) -> String
helpInfo {header, desc, footer, handleError} p errors =
  List.intercalate "\n\n" $ catMaybes
    [ header
    , map (List.intercalate "\n" <<< splitWords 50) desc
    , Just (helpDoc p)
    , map (List.intercalate "\n" <<< splitWords 50) footer
    ] <> helpErrors handleError errors

-- | A pretty-printed list of recognized environment variables suitable for usage messages
helpDoc :: Parser e a -> String
helpDoc p =
  List.intercalate "\n" ("Available environment variables:\n" : helpParserDoc p)

helpParserDoc :: Parser e a -> Array String
helpParserDoc =
  concat <<< Map.elems <<< foldAlt (\v -> Map.singleton (varfName v) (helpVarfDoc v)) <<< unParser

helpVarfDoc :: VarF e a -> Array String
helpVarfDoc VarF {varfName, varfHelp, varfHelpDef} =
  case varfHelp of
    Nothing -> [indent 2 varfName]
    Just h
      | k > 15    -> indent 2 varfName : map (indent 25) (splitWords 30 t)
      | otherwise ->
          case zipWith indent (23 - k : repeat 25) (splitWords 30 t) of
            (x : xs) -> (indent 2 varfName <> x) : xs
            []       -> [indent 2 varfName]
     where k = length varfName
           t = maybe h (\s -> h <> " (default: " <> s ++")") varfHelpDef

splitWords :: Int -> String -> Array String
splitWords n =
  go Nil 0 <<< words
 where
  go acc _ Nil = prep acc
  go acc k (w : ws)
    | k + z < n = go (w : acc) (k + z) ws
    | z > n     = prep acc <> case splitAt n w of (Tuple w' w'') -> w' : go Nil 0 (w'' : ws)
    | otherwise = prep acc <> go [w] z ws
   where
    z = length w

  prep []  = []
  prep acc = [unwords (reverse acc)]

indent :: Int -> String -> String
indent n s =
  replicate n " " <> s

helpErrors :: ErrorHandler -> Array (Tuple String e) -> Array String
helpErrors _       [] = []
helpErrors handler fs =
  [ "Parsing errors:"
  , List.intercalate "\n" (mapMaybe (uncurry handler) (List.sortBy (comparing fst) fs))
  ]

-- | Parser's metadata
type Info =
  { header      :: Maybe String
  , desc        :: Maybe String
  , footer      :: Maybe String
  , handleError :: ErrorHandler
  }

-- | Given a variable name and an error value, try to produce a useful error message
type ErrorHandler = String -> EnvError -> Maybe String

defaultInfo :: Info EnvError
defaultInfo = Info
  { header = Nothing
  , desc = Nothing
  , footer = Nothing
  , handleError = defaultErrorHandler
  }

-- | The default error handler
defaultErrorHandler :: ErrorHandler
defaultErrorHandler name err =
  oneOf [handleUnsetError name err, handleEmptyError name err, handleUnreadError name err]

handleUnsetError :: ErrorHandler
handleUnsetError name =
  map (\_ -> indent 2 (name <> " is unset")) <<< tryUnset

handleEmptyError :: ErrorHandler
handleEmptyError name =
  map (\_ -> indent 2 (name <> " is empty")) <<< tryEmpty

handleUnreadError :: ErrorHandler
handleUnreadError name =
  map (\val -> indent 2 (name <> " has value " <> val <> " that cannot be parsed")) <<< tryUnread

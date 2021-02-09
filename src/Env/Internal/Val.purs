module Env.Internal.Val where

import Data.Either (Either(..), either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus)

-- | A type isomorphic to 'Either' with the accumulating 'Applicative' instance.
data Val e a
  = Err e
  | Ok  a

derive instance functorVal :: Functor (Val a)
derive instance eqVal :: (Eq e, Eq a) => Eq (Val e a)
derive instance genericVal :: Generic (Val e a) _
instance showVal :: (Show e, Show a) => Show (Val e a) where show x = genericShow x

instance applyVal :: Semigroup e => Apply (Val e) where
  apply (Err e) (Err e') = Err (e <> e')
  apply (Err e) _        = Err e
  apply _       (Err e') = Err e'
  apply (Ok  f) (Ok  a)  = Ok (f a)

instance applicativeVal :: Semigroup e => Applicative (Val e) where
  pure = Ok

instance altVal :: Alt (Val e) where
  alt (Err _) r = r
  alt l       _ = l

instance plusVal :: Monoid e => Plus (Val e) where
  empty = Err mempty

instance alternativeVal :: Monoid e => Alternative (Val e)

fromEither :: forall e a . Either e a -> Val e a
fromEither = either Err Ok

toEither :: forall e a . Val e a -> Either e a
toEither x =
  case x of
       Err e -> Left e
       Ok a -> Right a

module Env.Internal.Free where

import Prelude

import Control.Alt (alt)
import Control.Alternative (class Alt, class Alternative, class Plus, empty)
import Data.Array (fold)
import Data.Exists (Exists)
import Data.Exists as Exists
import Data.Newtype (class Newtype, unwrap)

data AltAp f a i = AltAp (Alt f (i -> a)) (Alt f i)

data Alt f a
  = Nope
  | Pure a
  | Ap   (Exists (AltAp f a))
  | Alt  (Alt f a) (Alt f a)
  | Lift (f a)

-- | Print the free structure
inspect :: forall f a . Alt f a -> String
inspect Nope      = "Nope"
inspect (Pure _)  = "Pure _"
inspect (Ap exists) =
  Exists.runExists
  (\(AltAp f x) -> fold ["(", inspect f, ") <*> (", inspect x, ")"])
  exists
inspect (Alt x y) = fold ["(", inspect x, ") <|> (", inspect y, ")"]
inspect (Lift _)  = "Lift _"

instance functorAlt :: Functor f => Functor (Alt f) where
  map _ Nope      = Nope
  map f (Pure a)  = Pure (f a)
  map f (Ap exists)  =
    Exists.runExists
    (\(AltAp a v) -> Ap $ Exists.mkExists $ AltAp (map (\a' -> f <<< a') a) v)
    exists
  map f (Alt a b) = Alt (map f a) (map f b)
  map f (Lift a)  = Lift (map f a)

instance applyAlt :: Functor f => Apply (Alt f) where
  apply f a = Ap $ Exists.mkExists $ AltAp f a

instance applicativeAlt :: Functor f => Applicative (Alt f) where
  pure = Pure

instance altAlt :: Functor f => Alt (Alt f) where
  alt = Alt

instance plusAlt :: Functor f => Plus (Alt f) where
  empty = Nope

instance alternativeAlt :: Functor f => Alternative (Alt f)

liftAlt :: forall f a . f a -> Alt f a
liftAlt = Lift

runAlt :: forall f g a. Alternative g => (forall x. f x -> g x) -> Alt f a -> g a
runAlt u = go where
  go  :: forall b . Alt f b -> g b
  go Nope      = empty
  go (Pure a)  = pure a
  go (Ap exists) =
    Exists.runExists
    (\(AltAp f x) -> apply (go f) (go x))
    exists
  go (Alt s t) = alt (go s) (go t)
  go (Lift x)  = u x

foldAlt :: forall f b p . Monoid p => (forall a. f a -> p) -> Alt f b -> p
foldAlt f =
  unwrap <<< runAlt (Mon <<< f)

hoistAlt :: forall f g b. Functor g => (forall a. f a -> g a) -> Alt f b -> Alt g b
hoistAlt nat =
  runAlt (Lift <<< nat)

-- | The 'Alt' functor induced by the 'Monoid'
newtype Mon m a = Mon m

derive instance newtypeParser :: Newtype (Mon m a) _
derive instance functorMon :: Functor (Mon m)

instance applyMon :: Semigroup m => Apply (Mon m) where
  apply (Mon x) (Mon y) = Mon (append x y)

instance applicativeMon :: Monoid m => Applicative (Mon m) where
  pure _ = Mon mempty

instance altMon :: Semigroup m => Alt (Mon m) where
  alt (Mon x) (Mon y) = Mon (append x y)

instance plusMon :: Monoid m => Plus (Mon m) where
  empty = Mon mempty

instance alternativeMon :: Monoid m => Alternative (Mon m)


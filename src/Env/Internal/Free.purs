module Env.Internal.Free where

import Control.Alternative.Free (FreeAlternative, foldFreeAlternative)
import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus)
import Data.Newtype (class Newtype, unwrap)

foldMonoidFreeAlternative :: forall p f b . Monoid p => (forall a. f a -> p) -> FreeAlternative f b -> p
foldMonoidFreeAlternative f =
  unwrap <<< foldFreeAlternative (Mon <<< f)

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


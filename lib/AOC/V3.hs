{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Rank2Types #-}

module AOC.V3 (
  V3 (..),
  zero,
  (.+.),
  (.-.),
  (.*),
  sum,
  zipWith,
  zipWith3,
  mapWithIndex,
  fromV2,
  manhattan,
  boundingBox,
  fromList,
) where

import GHC.Ix (Ix (inRange, range, unsafeIndex, unsafeRangeSize))
import Prelude hiding (sum, zipWith, zipWith3)

import qualified AOC.V2 as V2
import qualified Data.Foldable as F

data V3 a = V3 {v3X ∷ !a, v3Y ∷ !a, v3Z ∷ !a}
  deriving (Eq, Foldable, Functor, Ord, Show)

fromList ∷ [a] → V3 a
fromList [x, y, z] = V3 x y z
fromList _ = error "AOC.V3.fromList: input must have length 3"

zero ∷ Num a ⇒ V3 a
zero = V3 0 0 0

(.+.) ∷ Num a ⇒ V3 a → V3 a → V3 a
(.+.) = zipWith (+)

infixl 6 .+.

(.-.) ∷ Num a ⇒ V3 a → V3 a → V3 a
(.-.) = zipWith (-)

infixl 6 .-.

(.*) ∷ Num a ⇒ V3 a → a → V3 a
V3 x1 y1 z1 .* s = V3 (x1 * s) (y1 * s) (z1 * s)

infixl 7 .*

sum ∷ (Foldable f, Num a) ⇒ f (V3 a) → V3 a
sum = F.foldl' (.+.) zero

mapCoord ∷ (a → b) → V3 a → V3 b
mapCoord f (V3 x y z) = V3 (f x) (f y) (f z)

zipWith ∷ (a → b → c) → V3 a → V3 b → V3 c
zipWith f (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (f x1 x2) (f y1 y2) (f z1 z2)
{-# INLINE zipWith #-}

zipWith3 ∷ (a → b → c → d) → V3 a → V3 b → V3 c → V3 d
zipWith3 f (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x3 y3 z3) =
  V3 (f x1 x2 x3) (f y1 y2 y3) (f z1 z2 z3)
{-# INLINE zipWith3 #-}

mapWithIndex ∷ ((∀ e. V3 e → e) → a → b) → V3 a → V3 b
mapWithIndex f (V3 x y z) = V3 (f (.v3X) x) (f (.v3Y) y) (f (.v3Z) z)
{-# INLINE mapWithIndex #-}

-- | Project a V2 vector on the Z=0 plane.
fromV2 ∷ Num a ⇒ V2.V2 a → V3 a
fromV2 v2 = V3 (v2.v2X) (v2.v2Y) 0

manhattan ∷ Num a ⇒ V3 a → V3 a → a
manhattan (V3 x1 y1 z1) (V3 x2 y2 z2) =
  abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

{- | find the upper-left and lower-right coords that inclusively contain all the
  coordinates in a list of coordinates
-}
boundingBox ∷ ∀ a. Ord a ⇒ [V3 a] → Maybe (V3 a, V3 a)
boundingBox = \case
  [] → Nothing
  V3 x y z : v3s → go x y z x y z v3s
  where
    go lox loy loz hix hiy hiz = \case
      [] → lo `seq` hi `seq` Just (lo, hi)
        where
          lo = V3 lox loy loz
          hi = V3 hix hiy hiz
      V3 x' y' z' : v3s' →
        go (min lox x') (min loy y') (min loz z') (max hix x') (max hiy y') (max hiz z') v3s'

instance Num a ⇒ Num (V3 a) where
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) = zipWith (*)
  negate = mapCoord negate
  abs = mapCoord abs
  signum = mapCoord signum
  fromInteger = (\i → V3 i i i) . fromInteger

instance Ix a ⇒ Ix (V3 a) where
  range (V3 l1 l2 l3, V3 u1 u2 u3) =
    [ V3 i1 i2 i3
    | i1 ← range (l1, u1)
    , i2 ← range (l2, u2)
    , i3 ← range (l3, u3)
    ]

  unsafeIndex (V3 l1 l2 l3, V3 u1 u2 u3) (V3 i1 i2 i3) =
    unsafeIndex (l3, u3) i3
      + unsafeRangeSize (l3, u3)
        * ( unsafeIndex (l2, u2) i2
              + unsafeRangeSize (l2, u2)
                * unsafeIndex (l1, u1) i1
          )

  inRange (V3 l1 l2 l3, V3 u1 u2 u3) (V3 i1 i2 i3) =
    inRange (l1, u1) i1
      && inRange (l2, u2) i2
      && inRange (l3, u3) i3

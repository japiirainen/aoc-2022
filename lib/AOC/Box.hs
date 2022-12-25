{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE StandaloneDeriving #-}

module AOC.Box where

import AOC.Nat (Nat (S, Z))
import Control.Monad (foldM)
import Data.Kind (Type)
import Data.List (foldl1')
import GHC.Stack (HasCallStack)

data Box ∷ Nat → Type where
  Pt ∷ Box 'Z
  Dim ∷ !Int → !Int → Box n → Box ('S n)

deriving instance Show (Box n)
deriving instance Eq (Box n)
deriving instance Ord (Box n)

size ∷ Box n → Int
size Pt = 1
size (Dim lo hi box) = (hi - lo) * size box

intersectBox ∷ Box n → Box n → Maybe (Box n)
intersectBox Pt Pt = Just Pt
intersectBox (Dim a b xs) (Dim c d ys) =
  [Dim x y zs | let x = max a c, let y = min b d, x < y, zs ← intersectBox xs ys]

intersectBoxes ∷ HasCallStack ⇒ [Box n] → Maybe (Box n)
intersectBoxes [] = error "intersectBoxes: empty intersection"
intersectBoxes (x : xs) = foldM intersectBox x xs

subtractBox ∷ Box n → Box n → [Box n]
subtractBox b1 b2 =
  case intersectBox b1 b2 of
    Nothing → [b2]
    Just b → subtractBox' b b2

subtractBox' ∷ Box n → Box n → [Box n]
subtractBox' Pt Pt = []
subtractBox' (Dim a b xs) (Dim c d ys) =
  [Dim c a ys | c < a]
    ++ [Dim b d ys | b < d]
    ++ [Dim a b zs | zs ← subtractBox' xs ys]

unionBox ∷ Box n → Box n → Box n
unionBox (Dim a b x) (Dim c d y) = Dim (min a c) (max b d) (unionBox x y)
unionBox Pt Pt = Pt

unionBoxes ∷ [Box n] → Box n
unionBoxes = foldl1' unionBox

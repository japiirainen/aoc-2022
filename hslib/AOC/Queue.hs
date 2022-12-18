{-# LANGUAGE PatternSynonyms #-}

module AOC.Queue where

import Data.Foldable (foldl', toList)

data Queue a = Queue [a] [a] !Int

pattern Empty :: Queue a
pattern Empty <- Queue [] _ _
  where
    Empty = Queue [] [] 0

pattern (:<|) :: a -> Queue a -> Queue a
pattern x :<| xs <- (pop -> Just (x, xs))

{-# COMPLETE Empty, (:<|) #-}

(|>) :: Queue a -> a -> Queue a
queue |> x = snoc x queue

instance Foldable Queue where
  null (Queue l _ _) = null l
  length (Queue l r _) = length l + length r
  elem x (Queue l r _) = elem x l || elem x r
  sum (Queue l r _) = sum l + sum r
  foldMap _ (Queue [] _ _) = mempty
  foldMap f (Queue (x : l) r 0) = f x <> rot l r
    where
      rot [] (y : _) = f y
      rot (x' : xs) (y : ys) = f x' <> rot xs ys <> f y
      rot _ _ = error "PANIC: Advent.Queue invariant violated"
  foldMap f (Queue (x : l) r i) = f x <> foldMap f (Queue l r (i - 1))

instance Show a => Show (Queue a) where
  showsPrec p q =
    showParen (p >= 11) $
      showString "fromList "
        . shows (toList q)

singleton :: a -> Queue a
singleton x = Queue [x] [] 1

fromList :: [a] -> Queue a
fromList l = Queue l [] (length l)

appendList :: Queue a -> [a] -> Queue a
appendList = foldl' (|>)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue (x : f) r s) = Just (x, exec f r s)
pop _ = Nothing

snoc :: a -> Queue a -> Queue a
snoc x (Queue f r s) = exec f (x : r) s

exec :: [a] -> [a] -> Int -> Queue a
exec f r 0 = fromList (rotate f r [])
exec f r i = Queue f r (i - 1)

rotate :: [a] -> [a] -> [a] -> [a]
rotate [] (y : _) a = y : a
rotate (x : xs) (y : ys) a = x : rotate xs ys (y : a)
rotate _ _ _ = error "AOC.Queue.rotate: panic"
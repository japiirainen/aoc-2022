module Main where

import AOC.Main (simpleMain)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))

import qualified Data.Map as M

main ∷ IO ()
main = simpleMain $ \input →
  let fs = bottomUp (\case File s → Sum s; Dir → 0) $ fromZipper (simulate input)
      dirs = map snd . filter ((== Dir) . fst) $ toList fs
      used = getSum . snd $ value fs
      neededSpace = 30_000_000 - (70_000_000 - used)
   in ( getSum . mconcat $ filter (<= 100_000) dirs
      , minimum (filter (>= neededSpace) (map getSum dirs))
      )

data FsNode = Dir | File Int
  deriving stock (Show, Eq)

simulateStep ∷ FsZipper String FsNode → [String] → FsZipper String FsNode
simulateStep z ["$", "cd", "/"] = top z
simulateStep z ["$", "cd", ".."] = fromMaybe (error "cd .. from root") (up z)
simulateStep z ["$", "cd", dir] = fromMaybe (error ("cd into " <> dir)) (down dir z)
simulateStep z ["$", "ls"] = z
simulateStep z ["dir", dir] = fromMaybe z (insert dir (singleton Dir) z)
simulateStep z [n, file] = fromMaybe z (insert file (singleton (File (read @Int n))) z)
simulateStep _ command = error ("invalid command" <> unlines command)

simulate ∷ String → FsZipper String FsNode
simulate = foldl' (\z line → simulateStep z (words line)) (toZipper (singleton Dir)) . lines

bottomUp ∷ ∀ k v m. (Monoid m, Ord k) ⇒ (v → m) → Fs k v → Fs k (v, m)
bottomUp f (Fs v kids) = Fs (v, total) kids'
  where
    kids' = bottomUp f <$> kids
    total = f v <> mconcat (map (\(_, Fs (_, x) _) → x) (M.toList kids'))

data Fs k v = Fs v (M.Map k (Fs k v))
  deriving stock (Eq, Foldable, Functor, Show)

type FsZipper k v = ([(Fs k v, k)], Fs k v)

singleton ∷ ∀ k v. v → Fs k v
singleton v = Fs v M.empty

value ∷ ∀ k v. Fs k v → v
value (Fs v _) = v

toZipper ∷ ∀ k v. Fs k v → FsZipper k v
toZipper fs = ([], fs)

fromZipper ∷ ∀ k v. Ord k ⇒ FsZipper k v → Fs k v
fromZipper = snd . top

up ∷ ∀ k v. Ord k ⇒ FsZipper k v → Maybe (FsZipper k v)
up = \case
  ([], _) → Nothing
  ((Fs pv kids, k) : ps, c) → Just (ps, Fs pv (M.insert k c kids))

top ∷ ∀ k v. Ord k ⇒ FsZipper k v → FsZipper k v
top z = maybe z top (up z)

down ∷ ∀ k v. Ord k ⇒ k → FsZipper k v → Maybe (FsZipper k v)
down k (ps, Fs pv kids) = do
  c ← M.lookup k kids
  pure ((Fs pv (M.delete k kids), k) : ps, c)

insert ∷ ∀ k v. Ord k ⇒ k → Fs k v → FsZipper k v → Maybe (FsZipper k v)
insert k c (ps, Fs pv kids) = maybe (Just (ps, Fs pv (M.insert k c kids))) (const Nothing) (M.lookup k kids)

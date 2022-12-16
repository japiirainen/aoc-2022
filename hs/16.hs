module Main where

import AOC.Main (pureMain)
import Control.Applicative ((<|>))
import Data.List (tails)

import qualified AOC.Parser as P
import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Set as Set

type Cave = (String, Int, [String])

parseCave ∷ P.Parser Char Cave
parseCave =
  (,,)
    <$> (P.string "Valve " *> P.many1 P.upper)
    <*> (P.string " has flow rate=" *> P.decimal <* P.char ';')
    <*> (P.many1 (Monad.void P.lower <|> P.char ' ') *> P.many1 P.upper `P.sepBy` P.string ", ")

pressures ∷ Map.Map String (Int, [String]) → Int → Map.Map (Set.Set String) Int
pressures graph = go [(("AA", Set.empty), 0)]
  where
    go states 0 = Map.fromListWith max (map (\((_, open), n) → (open, n)) states)
    go states t = go (simplify (concatMap step states)) (t - 1)
      where
        step ((here, open), n) =
          [((next, open), n) | next ← snd (graph Map.! here)]
            <> [ ((here, Set.insert here open), n + (t - 1) * amt)
               | Set.notMember here open
               , let amt = fst (graph Map.! here)
               , amt /= 0
               ]
    simplify = Map.assocs . Map.fromListWith max

main ∷ IO ()
main = pureMain $ \input → do
  caves ← P.runParser (parseCave `P.sepBy` P.newline) input
  let graph = Map.fromList ((\(k, n, vs) → (k, (n, vs))) <$> caves)
  let part2 = maximum do
        (open1, v1) : elephants ← tails (Map.assocs (pressures graph 26))
        (open2, v2) ← elephants
        Monad.guard $ Set.null (Set.intersection open1 open2)
        pure (v1 + v2)
  pure (pure (maximum $ pressures graph 30), pure part2)

module AOC.Search where

import qualified AOC.Queue as Queue
import qualified Data.Set as Set

bfs ∷ ∀ a. Ord a ⇒ (a → [a]) → a → [a]
bfs = bfsOn id

bfsN ∷ ∀ a. Ord a ⇒ (a → [a]) → [a] → [a]
bfsN = bfsOnN id

bfsOn ∷
  ∀ a r.
  Ord r ⇒
  (a → r) →
  (a → [a]) →
  a →
  [a]
bfsOn rep next start = bfsOnN rep next [start]

bfsOnN ∷
  ∀ a r.
  Ord r ⇒
  (a → r) →
  (a → [a]) →
  [a] →
  [a]
bfsOnN rep next start = loop Set.empty (Queue.fromList start)
  where
    loop !seen = \case
      Queue.Empty → []
      x Queue.:<| q
        | Set.member (rep x) seen → loop seen q
        | otherwise → x : loop seen' q'
        where
          seen' = Set.insert (rep x) seen
          q' = Queue.appendList q (next x)

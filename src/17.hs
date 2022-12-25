module Main where

import AOC.Main (simpleMain)
import Data.Set (Set)
import Data.Stream.Infinite (Stream (..))
import Linear.V2 (V2 (..))

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Stream.Infinite as Stream

type Solids = Set (V2 Int)

maxY ∷ Solids → Int
maxY = maximum . fmap (\(V2 _ b) → b) . Set.toList

initialSolids ∷ Solids
initialSolids = Set.fromList [V2 x 0 | x ← [0 .. 6]]

type Block = [V2 Int]
type Move = Int

blocks ∷ Stream (Int, Block)
blocks =
  Stream.cycle . NE.fromList . zip [0 ..] $
    [ [V2 0 0, V2 1 0, V2 2 0, V2 3 0]
    , [V2 1 2, V2 0 1, V2 1 1, V2 2 1, V2 1 0]
    , [V2 2 2, V2 2 1, V2 0 0, V2 1 0, V2 2 0]
    , [V2 0 3, V2 0 2, V2 0 1, V2 0 0]
    , [V2 0 1, V2 1 1, V2 0 0, V2 1 0]
    ]

move ∷ Move → Solids → Block → Block
move m solids block =
  let next = map (\(V2 x y) → V2 (min 6 . max 0 $ x + m) y) block
   in if any (\(x, y) → x == y || y `Set.member` solids) (zip block next)
        then block
        else next

simulateBlock ∷ Block → Stream (Int, Move) → Solids → (Stream (Int, Move), Solids)
simulateBlock block moves solids = go initPos moves
  where
    topY = maxY solids + 4
    initPos = map (+ V2 2 topY) block
    go bl (m :> ms)
      | any (`Set.member` solids) bl'' = (ms, solids `Set.union` Set.fromList bl')
      | otherwise = go bl'' ms
      where
        bl' = move (snd m) solids bl
        bl'' = map (+ V2 0 (-1)) bl'

simulate ∷ Stream (Int, Block) → Stream (Int, Move) → Solids → Stream (Int, Int, Int, Solids)
simulate = go 0
  where
    go !n (bl :> bls) moves@(m :> _) solids = st :> go (n + 1) bls moves' solids'
      where
        st = (n, fst bl, fst m, solids)
        (moves', solids') = simulateBlock (snd bl) moves solids

part1 ∷ Stream (Int, Move) → Int
part1 moves =
  let states = simulate blocks moves initialSolids
      ((_, _, _, solids) :> _) = Stream.filter (\(n, _, _, _) → n == 2022) states
   in maxY solids

findRepeat ∷ ∀ a b. Ord b ⇒ (a → b) → Stream a → a
findRepeat f = go Map.empty
  where
    go seen (x :> xs) =
      let fx = f x
       in case Map.lookup fx seen of
            Nothing → go (Map.insert fx x seen) xs
            Just y → y

part2 ∷ Stream (Int, Move) → Int
part2 moves = height * cycles + tx
  where
    states = simulate blocks moves initialSolids
    (_, bi, mi, _) = findRepeat (\(_, x, y, _) → (x, y)) states
    statesR = Stream.filter (\(_, x, y, _) → x == bi && y == mi) states
    ((n1, _, _, solids) :> (n2, _, _, maxY → t2) :> (n3, _, _, maxY → t3) :> _) = statesR
    height = t3 - t2
    period = n3 - n2
    (cycles, extra) = (1000000000000 - n1) `divMod` period
    statesX = simulate (Stream.drop bi blocks) (Stream.drop mi moves) solids
    ((_, _, _, maxY → tx) :> _) = Stream.filter (\(n, _, _, _) → n == extra) statesX

main ∷ IO ()
main = simpleMain $ \input →
  let parse =
        Stream.cycle
          . NE.fromList
          . zip [0 ..]
          . map (\case '>' → 1; _ → -1)
          . head
          . lines
      moves = parse input
   in (part1 moves, part2 moves)

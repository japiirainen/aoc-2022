module Main where

import AOC.Main (pureMain)
import Control.Applicative (Alternative (..))
import Data.List (find, sort)
import Data.Maybe (isJust)

import qualified AOC.Parser as P
import qualified Control.Monad as Monad

data Packet = I Int | L [Packet]
  deriving stock (Show, Eq)

instance Ord Packet where
  compare (I a) (I b) = compare a b
  compare (L (x : xs)) (L (y : ys)) = case compare x y of
    EQ → compare (L xs) (L ys)
    c → c
  compare (L []) (L (_ : _)) = LT
  compare (L (_ : _)) (L []) = GT
  compare (L []) (L []) = EQ
  compare l@(L _) (I i) = compare l (L [I i])
  compare (I i) l@(L _) = compare (L [I i]) l

parsePacket ∷ P.Parser Char Packet
parsePacket =
  let list = P.char '[' *> parsePacket `P.sepBy` P.char ',' <* P.char ']'
   in (L <$> list <|> I <$> P.decimal) <* P.spaces

main ∷ IO ()
main = pureMain $ \input → do
  pairs ← P.runParser (many $ (,) <$> parsePacket <*> parsePacket) input
  let part1 = sum do
        (i, (a, b)) ← zip [1 ∷ Int ..] pairs
        Monad.guard $ a < b
        pure i
      dividers = [L [L [I 2]], L [L [I 6]]]
      packets = sort (dividers <> [p | (a, b) ← pairs, p ← [a, b]])
      part2 = product do
        (i, p) ← zip [1 ∷ Int ..] packets
        Monad.guard $ isJust $ find (p ==) dividers
        pure i
  pure (pure part1, pure part2)

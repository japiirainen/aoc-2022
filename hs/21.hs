module Main where

import AOC.Main (defaultMain)
import Control.Applicative (optional, (<|>))
import Data.Foldable (traverse_)
import Data.Functor (($>))

import qualified AOC.Parser as P
import qualified Data.Map as Map
import qualified Data.SBV as SBV

data Op
  = Mul
  | Add
  | Div
  | Sub

data Expr
  = Lit Int
  | Var String
  | BinOp Expr Op Expr

newtype Monkeys = Monkeys {unMonkeys ∷ Map.Map String Expr}

parseMonkeys ∷ P.Parser Char Monkeys
parseMonkeys = Monkeys . Map.fromList <$> P.sepBy1 monkey P.newline
  where
    monkey = (,) <$> (var <* P.char ':' <* P.spaces) <*> expr
    expr =
      ( \lhs mb → case mb of
          Nothing → lhs
          Just (o, rhs) → BinOp lhs o rhs
      )
        <$> term
        <*> optional ((,) <$> op <*> term)
    term = (Var <$> var <|> Lit <$> P.signedDecimal) <* P.horizontalSpaces
    var = P.many1 P.lower
    op =
      ( P.char '+' $> Add
          <|> P.char '*' $> Mul
          <|> P.char '-' $> Sub
          <|> P.char '/' $> Div
      )
        <* P.horizontalSpaces

part1 ∷ String → Monkeys → Int
part1 name (Monkeys ms) = mm Map.! name
  where
    mm ∷ Map.Map String Int
    mm = Map.fromList [(n, eval (ms Map.! n)) | n ← Map.keys ms]
    eval ∷ Expr → Int
    eval = \case
      Lit n → n
      Var n → mm Map.! n
      BinOp l op r → case op of
        Mul → eval l * eval r
        Add → eval l + eval r
        Div → eval l `div` eval r
        Sub → eval l - eval r

part2 ∷ Monkeys → IO SBV.SatResult
part2 (Monkeys ms) = SBV.sat do
  humn ← SBV.sbvExists "humn"
  let m ∷ Map.Map String SBV.SInteger
      m = Map.fromList [(n, if n == "humn" then humn else eval e) | (n, e) ← Map.toList ms]
      eval = \case
        Lit n → SBV.literal (fromIntegral n) ∷ SBV.SInteger
        Var n → m Map.! n
        BinOp l op r → case op of
          Mul → eval l * eval r
          Add → eval l + eval r
          Div → eval l `SBV.sDiv` eval r
          Sub → eval l - eval r

  sequence_
    [ SBV.constrain (SBV.sMod (m Map.! a) (m Map.! b) SBV..== 0)
    | (_, BinOp (Var a) Div (Var b)) ← Map.toList ms
    ]
  case Map.lookup "root" ms of
    Just (BinOp (Var r1) _ (Var r2)) → pure (m Map.! r1 SBV..== m Map.! r2)
    _ → fail "missing root"

main ∷ IO ()
main = defaultMain $ \h → do
  monkeys ← P.hRunParser h parseMonkeys
  result ← part2 monkeys
  pure
    ( print (part1 "root" monkeys)
    , traverse_ print (SBV.getModelValue "humn" result ∷ Maybe Integer)
    )

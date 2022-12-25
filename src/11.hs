module Main where

import AOC.Main (pureMain)
import Control.Applicative (Alternative ((<|>)))
import Data.List (foldl', sort)

import qualified AOC.Parser as P
import qualified Data.Map as M

main ∷ IO ()
main = pureMain $ \input → do
  monkeys ← P.runParser (parseMonkey `P.sepBy` (P.newline <* P.newline)) input
  let m0 = M.fromList $ (\m → (m.mId, m)) <$> monkeys
      solve modulus rounds =
        product . take 2 . reverse . sort $
          M.elems ((.throws) <$> runTimes rounds (execRound modulus) m0)
      magic = foldl1 lcm (map (.divisor) monkeys)
  pure (pure (solve (`div` 3) 20), pure (solve (`mod` magic) 10_000))

data Monkey a = Monkey
  { mId ∷ a
  , items ∷ [a]
  , fn ∷ a → a
  , divisor ∷ a
  , onTrue ∷ a
  , onFalse ∷ a
  , throws ∷ a
  }

parseMonkey ∷ P.Parser Char (Monkey Int)
parseMonkey = mkMonkey <$> pId <*> pItems <*> pOp <*> pDivisor <*> pIfTrue <*> pIfFalse
  where
    mkMonkey i items (op, arg) divisor ifTrue ifFalse = Monkey i items (evalOp op arg) divisor ifTrue ifFalse 0
    pId = P.string "Monkey " *> P.decimal <* P.char ':' <* P.newline
    pItems = P.spaces *> P.string "Starting items: " *> P.decimal `P.sepBy` P.string ", " <* P.newline
    pOp = P.spaces *> P.string "Operation: new = old " *> ((,) <$> P.anyChar <* P.space <*> pOpArg)
    pOpArg = Nothing <$ P.string "old" <|> Just <$> P.decimal
    pDivisor = P.spaces *> P.string "Test: divisible by " *> P.decimal <* P.newline
    pIfTrue = P.spaces *> P.string "If true: throw to monkey " *> P.decimal <* P.newline
    pIfFalse = P.spaces *> P.string "If false: throw to monkey " *> P.decimal

evalOp ∷ ∀ a. Num a ⇒ Char → Maybe a → a → a
evalOp '*' (Just x) y = x * y
evalOp '+' (Just x) y = x + y
evalOp '*' Nothing y = y * y
evalOp '+' Nothing y = y + y
evalOp _ _ _ = error "Invalid operation"

execRound ∷ (Int → Int) → M.Map Int (Monkey Int) → M.Map Int (Monkey Int)
execRound modulus monkeys = foldl' (execMonkey modulus) monkeys (M.keys monkeys)

execMonkey ∷ (Int → Int) → M.Map Int (Monkey Int) → Int → M.Map Int (Monkey Int)
execMonkey wrf monkeys i = case (monkeys M.! i).items of
  [] → monkeys
  (x : xs) → execMonkey wrf monkeys' i
    where
      m = monkeys M.! i
      val = wrf (m.fn x)
      destination = if val `mod` m.divisor == 0 then m.onTrue else m.onFalse
      monkeys' =
        M.adjust (\m' → m' {items = [val] <> m'.items}) destination $
          M.adjust (\n → n {items = xs, throws = m.throws + 1}) i monkeys

runTimes ∷ ∀ a. Int → (a → a) → a → a
runTimes 0 _ x = x
runTimes n f x = runTimes (n - 1) f $! f x

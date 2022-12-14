{-# LANGUAGE FlexibleInstances #-}

module AOC.Main (
  defaultMain,
  pureMain,
  simpleMain,
) where

import Data.Bifunctor (bimap)
import Data.Word (Word64)
import System.Environment (getArgs, getProgName)
import qualified System.IO as IO
import Text.Read (readMaybe)

data Part = Part1 | Part2 deriving (Eq)

parsePart ∷ String → Either String Part
parsePart str = case readMaybe str ∷ Maybe Int of
  Just 1 → Right Part1
  Just 2 → Right Part2
  _ → Left $ "Invalid part: " <> str

-- | 'defaultMain' is the most general main function.
defaultMain ∷ (IO.Handle → IO (IO (), IO ())) → IO ()
defaultMain f = do
  args ← getArgs
  case args of
    [] → do
      (pt1, pt2) ← f IO.stdin
      pt1
      pt2
    [path] → IO.withFile path IO.ReadMode $ \h → do
      (pt1, pt2) ← f h
      pt1
      pt2
    [_day, part, path] → case parsePart part of
      Left err → fail err
      Right p → IO.withFile path IO.ReadMode $ \h → do
        (pt1, pt2) ← f h
        case p of
          Part1 → pt1
          Part2 → pt2
    _ → do
      progName ← getProgName
      IO.hPutStrLn IO.stderr $
        unlines
          [ "Usage: "
          , ""
          , "  " <> progName
          , ""
          , "    Run both parts using stdin as input."
          , ""
          , "  " <> progName <> " DAY PART PATH"
          , ""
          , "    DAY is ignored but present to confirm to the spec."
          , "    PART is 1 or 2."
          , "    PATH is the path to the input file."
          ]

class Solution a where
  printSolution ∷ a → IO ()

instance Solution () where
  printSolution _ = pure ()

instance Solution Int where
  printSolution = print

instance Solution Integer where
  printSolution = print

instance Solution Word64 where
  printSolution = print

instance Solution [Char] where
  printSolution = putStrLn

instance Solution a ⇒ Solution (Maybe a) where
  printSolution = maybe (pure ()) printSolution

-- | 'pureMain' avoids using 'IO' and rather uses 'Either' for error handling.
pureMain ∷
  (Solution a, Solution b) ⇒
  (String → Either String (Either String a, Either String b)) →
  IO ()
pureMain f = defaultMain $ \handle → do
  str ← IO.hGetContents handle
  parts ← either fail pure $ f str
  return $ bimap (either fail printSolution) (either fail printSolution) parts

-- | 'simpleMain' can be used in case there's no errors alltogether.
simpleMain ∷
  (Solution a, Solution b) ⇒ (String → (a, b)) → IO ()
simpleMain f = pureMain $ Right . bimap Right Right . f

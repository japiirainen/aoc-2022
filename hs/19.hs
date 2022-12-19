module Main where

import AOC.Main (pureMain)
import Data.List (nub)

import qualified AOC.Parser as P
import qualified Data.Set as Set

data Blueprint = Blueprint
  { _id ∷ !Int
  , oreCost ∷ !Int
  , clayCost ∷ !Int
  , obsidianOreCost ∷ !Int
  , obsidianClayCost ∷ !Int
  , geodeOreCost ∷ !Int
  , geodeObsidianCost ∷ !Int
  }

parseBlueprints ∷ P.Parser Char [Blueprint]
parseBlueprints = parseBlueprint `P.sepBy` P.newline
  where
    parseBlueprint =
      Blueprint
        <$> (P.string "Blueprint " *> P.decimal <* P.string ": ")
        <*> (P.string "Each ore robot costs " *> P.decimal <* P.string " ore. ")
        <*> (P.string "Each clay robot costs " *> P.decimal <* P.string " ore. ")
        <*> (P.string "Each obsidian robot costs " *> P.decimal <* P.string " ore ")
        <*> (P.string "and " *> P.decimal <* P.string " clay. ")
        <*> (P.string "Each geode robot costs " *> P.decimal <* P.string " ore ")
        <*> (P.string "and " *> P.decimal <* P.string " obsidian.")

data SolverState = SolverState
  { ore ∷ !Int
  , clay ∷ !Int
  , obsidian ∷ !Int
  , geode ∷ !Int
  , oreBots ∷ !Int
  , clayBots ∷ !Int
  , obsidianBots ∷ !Int
  , geodeBots ∷ !Int
  }
  deriving (Eq, Ord)

initialSolverState ∷ SolverState
initialSolverState =
  SolverState
    { ore = 0
    , clay = 0
    , obsidian = 0
    , geode = 0
    , oreBots = 1
    , clayBots = 0
    , obsidianBots = 0
    , geodeBots = 0
    }

stateRepr ∷ SolverState → (Int, Int, Int, Int)
stateRepr SolverState {..} = (oreBots, clayBots, obsidianBots, geodeBots)

solver ∷ Int → SolverState → Blueprint → Int
solver timeLimit state0 blueprint = go 0 (Set.singleton (0, 0, 0, 0)) [state0]
  where
    go time _ states | time == timeLimit = maximum (map (.geode) states)
    go time seen states = go (time + 1) (seen `Set.union` Set.fromList (map stateRepr states')) states'
      where
        states' =
          map tickState states
            <> filter
              (\x → stateRepr x `Set.notMember` seen)
              (nub (concatMap (actions blueprint) states))

tickState ∷ SolverState → SolverState
tickState state@SolverState {..} =
  state
    { ore = ore + oreBots
    , clay = clay + clayBots
    , obsidian = obsidian + obsidianBots
    , geode = geode + geodeBots
    }

actions ∷ Blueprint → SolverState → [SolverState]
actions Blueprint {..} state =
  [ state' {ore = state'.ore - geodeOreCost, obsidian = state'.obsidian - geodeObsidianCost, geodeBots = state'.geodeBots + 1}
  | geodeOreCost <= state.ore
  , geodeObsidianCost <= state.obsidian
  ]
    ~~ [ state' {ore = state'.ore - obsidianOreCost, clay = state'.clay - obsidianClayCost, obsidianBots = state'.obsidianBots + 1}
       | obsidianOreCost <= state.ore
       , obsidianClayCost <= state.clay
       , state.obsidianBots < obsidianBotMax
       ]
    ~~ [state' {ore = state'.ore - clayCost, clayBots = state'.clayBots + 1} | clayCost <= state.ore, state.clayBots < clayBotMax]
      <> [state' {ore = state'.ore - oreCost, oreBots = state'.oreBots + 1} | oreCost <= state.ore, state.oreBots < oreBotMax]
  where
    state' = tickState state
    oreBotMax = maximum [oreCost, clayCost, obsidianOreCost, geodeOreCost]
    clayBotMax = obsidianClayCost
    obsidianBotMax = geodeObsidianCost

-- | This is the core optimization that makes this solution (somewhat, around 400ms for both parts) fast
(~~) ∷ [a] → [a] → [a]
[] ~~ ys = ys
xs ~~ _ = xs

main ∷ IO ()
main = pureMain $ \input → do
  blueprints ← P.runParser parseBlueprints input
  pure
    ( pure $ sum (map (\bp@Blueprint {..} → _id * solver 24 initialSolverState bp) blueprints)
    , pure $ product (map (solver 32 initialSolverState) (take 3 blueprints))
    )

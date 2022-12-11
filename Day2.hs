module Main where

import Data.Bifunctor (bimap, first)
import Aoc

type Player1 = Choice
type Player2 = Choice

main :: IO ()
main = do
  content <- input 2
  let run = Aoc.run content
  run 14264 (strategy one)
  run 12382 (strategy two)
  where
    one = parse (\x -> (snd x, uncurry (flip outcome) x)) parseChoice parseChoice
    two = parse (\x -> first (decide (snd x)) x) parseChoice parseOutcome

strategy :: (String -> [(Choice, Outcome)]) -> String -> Int
strategy a = sum . map (uncurry score) . a

data Choice = Rock | Paper | Scissor
  deriving (Eq, Ord, Show, Enum)

data Outcome = Loss | Draw | Win
  deriving (Show, Enum)

outcome :: Player1 -> Player2 -> Outcome
outcome a b
  | a == b = Draw
  | beats a == b = Win
  | otherwise = Loss

beats :: Choice -> Choice
beats Rock = Scissor
beats a = pred a

loses :: Choice -> Choice
loses Scissor = Rock
loses a = succ a

choiceScore :: Choice -> Int
choiceScore = (+1) . fromEnum

outcomeScore :: Outcome -> Int
outcomeScore = (*3) . fromEnum

score :: Choice -> Outcome -> Int
score a b = choiceScore a + outcomeScore b

parseChoice :: Char -> Choice
parseChoice 'A' = Rock
parseChoice 'X' = Rock
parseChoice 'B' = Paper
parseChoice 'Y' = Paper
parseChoice 'C' = Scissor
parseChoice 'Z' = Scissor
parseChoice _ = error "malformed input"

-- for part2
parseOutcome :: Char -> Outcome
parseOutcome 'X' = Loss
parseOutcome 'Y' = Draw
parseOutcome 'Z' = Win
parseOutcome _ = error "malformed input"

parseTurn :: (Char -> a) -> (Char -> b) -> String -> (a, b)
parseTurn a b (x : _ : y : _) = bimap a b (x, y)

parse :: ((a, b) -> (Choice, Outcome)) -> (Char -> a) -> (Char -> b) -> String -> [(Choice, Outcome)]
parse a b c = map (a . parseTurn b c) . lines

decide :: Outcome -> Choice -> Choice
decide Win a = loses a
decide Draw a = a
decide Loss a = beats a

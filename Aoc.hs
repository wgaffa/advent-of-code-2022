module Aoc where

import Control.Exception (assert)

type Day = Int

input :: Int -> IO String
input day = readFile filepath
  where
    filepath = "inputs/day" ++ show day ++ ".txt"

sample :: Int -> IO String
sample day = readFile filepath
  where
    filepath = "inputs/day" ++ show day ++ "-sample.txt"

run :: (Eq a, Show a) => String -> a -> (String -> a) -> IO ()
run content solution f = do
  print $ assert (calculated == solution) calculated
  where
    calculated = f content

data Puzzle = Sample | Puzzle | AllPuzzles

data Part = Part1 | Part2 | AllParts

aoc :: Int -> (String -> Int) -> (String -> Int) -> Puzzle -> Part -> IO ()
aoc day part1 part2 AllPuzzles AllParts = do
  let run = aoc day part1 part2
  run AllPuzzles Part1
  run AllPuzzles Part2
aoc day part1 part2 AllPuzzles Part1 = do
  let run = aoc day part1 part2
  run Sample Part1
  run Puzzle Part1
aoc day part1 part2 AllPuzzles Part2 = do
  let run = aoc day part1 part2
  run Sample Part2
  run Puzzle Part2
aoc day part1 part2 Sample AllParts = do
  let run = aoc day part1 part2
  run Sample Part1
  run Sample Part2
aoc day part1 part2 Puzzle AllParts = do
  let run = aoc day part1 part2
  run Puzzle Part1
  run Puzzle Part2
aoc day part1 part2 Sample Part1 = do
  let run = aoc day part1 part2
  undefined
aoc day part1 part2 Sample Part2 = do
  let run = aoc day part1 part2
  undefined
aoc day part1 part2 Puzzle Part1 = do
  let run = aoc day part1 part2
  undefined
aoc day part1 part2 Puzzle Part2 = do
  let run = aoc day part1 part2
  undefined

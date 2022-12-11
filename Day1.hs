module Main where

import Data.List (sort)
import Aoc

type Calorie = Int

type Elf = [Calorie]

type Expedition = [Elf]

main :: IO ()
main = do
  content <- input 1
  let run = Aoc.run content
  run 69206 (part1 . lines)
  run 197400 (part2 . lines)
  
parse :: [String] -> Expedition
parse [] = []
parse xs = case break null . dropWhile null $ xs of
  ([], _) -> []
  (xs, rest) -> map read xs : parse rest where

part1 :: [String] -> Int
part1 = maximum . map sum . parse

part2 :: [String] -> Int
part2 = sum . take 3 . reverse . sort . map sum . parse

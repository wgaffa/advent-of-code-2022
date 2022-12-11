module Main where

import Data.Bifunctor (bimap, first, second)
import Data.Bits
import Data.Char (isAlpha, isUpper, ord)
import Data.List (intersect, nub, sort)

import Aoc

main :: IO ()
main = do
  content <- input 3
  let run = Aoc.run content
  run 7817 (strategy fault id . lines)
  run 2444 (strategy partTwo group . lines)
  where
    strategy a b = sum . map (priority . a) . b
    partTwo = head . foldl1 intersect

type Rucksack = [Item]

type Compartment = [Item]

type Item = Char

type Group = [Rucksack]

priority :: Item -> Int
priority x
  | not (isAlpha x) = error "malformed input"
  | isUpper x = lexical_ord + 26
  | otherwise = lexical_ord
  where
    lexical_ord = ord x .&. 0b11111

compartments :: String -> (Compartment, Compartment)
compartments xs = splitAt half xs
  where
    half = length xs `shiftR` 1

fault :: Rucksack -> Item
fault = head . uncurry intersect . compartments

group :: [Rucksack] -> [Group]
group [] = []
group (a : b : c : xs) = [a, b, c] : group xs

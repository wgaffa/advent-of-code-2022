module Main where

import Data.Bifunctor (bimap, first, second)
import Data.List

import Aoc

main :: IO ()
main = do
  content <- input 4
  let run = Aoc.run content
  run 456 (length . strategy fullOverlap)
  run 808 (length . strategy overlap)
  where
    strategy a = filter (uncurry a . parsePair) . lines

type Range = (Int, Int)

type Pair = (Range, Range)

fullOverlap :: Range -> Range -> Bool
fullOverlap x y = cmp x y || cmp y x
  where
    cmp a b = fst a <= fst b && snd a >= snd b

overlap :: Range -> Range -> Bool
overlap a b = cmp a b || cmp b a
  where
    cmp a b = fst a <= fst b && snd a >= fst b

parseRange :: String -> Range
parseRange = bimap read (negate . read) . break (=='-')

parsePair :: String -> Pair
parsePair = bimap parseRange (parseRange . drop 1) . break (==',')

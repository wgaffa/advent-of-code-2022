module Main where

import Data.Bifunctor (bimap, first, second)
import Data.List
import Data.Char
import Data.Foldable
import qualified Data.IntMap as IM
import Control.Monad.State
import Data.Functor.Identity

import Aoc

main :: IO ()
main = do
  input <- input 8
  Aoc.run input 1798 part1

  print $ part2 input
  Aoc.run input 259308 part2
  where
    defState = (IM.empty, 0)
    part1 = length . filter id . concat . matrixManipulation compareLeft (const True) (zipWith (||)) . lines
    part2 = maximum . concat . matrixManipulation leftView defState (zipWith (*)) . lines

type ComparisonState a b = State (b -> a) a
type RangeMap = (IM.IntMap Int, Int)

matrixManipulation :: (Int -> StateT s Identity a) -> s -> ([a] -> [a] -> [a]) -> [String] -> [[a]]
matrixManipulation p defState zipper ls = map (uncurry zipper) $ zip horizontal vertical
  where
    view f = map (parseLine f) $ ls
    left = run $ view p
    right = map reverse . run . map reverse $ view p 
    top = transpose . run . transpose $ view p
    bottom = transpose . map reverse . run . map reverse . transpose $ view p
    horizontal = map (uncurry zipper) $ zip left right
    vertical = map (uncurry zipper) $ zip top bottom
    run = map (\x -> fst . runIdentity $ runStateT (sequence x) defState)

findHighestNearest :: Int -> IM.IntMap Int -> Int
findHighestNearest i = maxElem . IM.elems . IM.filterWithKey (\k _ -> k >= i)
  where
    maxElem = foldr' (\x y -> if x > y then x else y) 0

leftView :: Int -> StateT RangeMap Identity Int
leftView x = do
  (treeIndexes, index) <- get
  let highest = findHighestNearest x treeIndexes
  let newTree = IM.insert x index treeIndexes
  let range = index - highest

  put (newTree, index + 1)
  pure range

compareLeft :: Int -> ComparisonState Bool Int
compareLeft x = do
  f <- get
  let cmp = f x
  if cmp then do
    put (>x)
    pure cmp
  else
    pure cmp

merge :: [Bool] -> [Bool] -> [Bool]
merge = zipWith (||)

parseLine :: (Int -> a) -> String -> [a]
parseLine f = map (f . digitToInt)

module Main where

import Data.Bifunctor (bimap, first, second)
import Data.List
import Data.Char
import Control.Monad.State
import Data.Functor.Identity

import Aoc

main :: IO ()
main = do
  input <- input 8
  Aoc.run input 1798 part1

  Aoc.run input 100464 part2 -- too low
  where
    part1 = length . filter id . concat . matrixManipulation compareLeft (const True) (zipWith (||)) . lines
    part2 = maximum . concat . matrixManipulation leftView (const (0, minBound :: Int, 1)) (zipWith (*)) . lines

type ComparisonState a b = State (b -> a) a

matrixManipulation :: (Int -> StateT p Identity a) -> p -> ([a] -> [a] -> [a]) -> [String] -> [[a]]
matrixManipulation p defState zipper ls = map (uncurry zipper) $ zip horizontal vertical
  where
    state f ff = map (parseLine f) . ff $ ls
    left = run $ state p id
    right = map reverse . run . map reverse $ state p id
    top = transpose . run $ state p transpose
    bottom = transpose . map reverse . run . map reverse $ state p transpose
    horizontal = map (uncurry zipper) $ zip left right
    vertical = map (uncurry zipper) $ zip top bottom
    run = map (\x -> fst . runIdentity $ runStateT (sequence x) defState)

-- Some intermediate test functions for ghci
runner :: [[StateT (Int -> (Int, Int, Int)) Identity a]] -> [[a]]
runner = map (\x -> fst . runIdentity $ runStateT (sequence x) (const (0, minBound :: Int, 1)))

left' :: [[StateT (Int -> (Int, Int, Int)) Identity a]] -> [[a]]
left' = runner

right' :: [[StateT (Int -> (Int, Int, Int)) Identity a]] -> [[a]]
right' = map reverse . runner . map reverse

top' :: [[StateT (Int -> (Int, Int, Int)) Identity a]] -> [[a]]
top' = transpose . runner

bottom' :: [[StateT (Int -> (Int, Int, Int)) Identity a]] -> [[a]]
bottom' = transpose . map reverse . runner . map reverse

horizontal' x y = map (uncurry (zipWith (*))) $ zip x y
vertical' x y =  map (uncurry (zipWith (*))) $ zip x y

both' x y = map (uncurry (zipWith (*))) $ zip x y
-- end of crap

leftView :: Int -> StateT (Int -> (Int, Int, Int)) Identity Int
leftView x = do
  f <- get
  let (add, max, counter) = f x
  put $ \a -> case (a > max, a > x) of
    (True, _) -> (counter, a, counter + 1)
    (False, True) -> (add + 1, max, counter + 1)
    (_, _) -> (1, max, counter + 1)
  pure add

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

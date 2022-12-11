module Main where

import Data.Bifunctor (bimap, first, second)
import Data.List
import Data.Char
import Data.Maybe
import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP

import Aoc

main :: IO ()
main = do
  content <- input 5
  let run = Aoc.run content
  run "CNSZFDVLJ" (strategy reverse)
  run "QNDWLMGNS" (strategy id)
  where
    strategy a = map head . uncurry (foldl' $ flip (runProcedure a)) . bimap parseStack procedures . sections

type Stack = [Crate]

type Crate = Char

data Procedure = Move Int Int Int
  deriving Show

liftCrate :: Int -> [Stack] -> (Crate, [Stack])
liftCrate i stacks = doLift (i - 1) stacks
  where
    doLift :: Int -> [Stack] -> (Crate, [Stack])
    doLift 0 (x:xs) = (head x, (tail x):xs)
    doLift i (x:xs) = second (x:) $ doLift (i - 1) xs

liftNCrate :: ([Crate] -> [Crate]) -> Int -> Int -> [Stack] -> ([Crate], [Stack])
liftNCrate liftStrat count i stacks = doLift (i - 1) stacks
  where
    doLift :: Int -> [Stack] -> ([Crate], [Stack])
    doLift 0 (x:xs) = (liftStrat . take count $ x, (drop count x):xs)
    doLift i (x:xs) = second (x:) $ doLift (i - 1) xs

dropCrate :: Int -> Stack -> [Stack] -> [Stack]
dropCrate i crates stacks = doDrop (i - 1) stacks
  where
    doDrop :: Int -> [Stack] -> [Stack]
    doDrop 0 (x:xs) = (crates ++ x) : xs
    doDrop i (x:xs) = x : doDrop (i - 1) xs
    doDrop _ [] = []

move :: ([Crate] -> [Crate]) -> Int -> Int -> Int -> [Stack] -> [Stack]
move f count from to stack = dropCrate to lifted remainder
  where
    (lifted, remainder) = liftNCrate f count from stack

runProcedure :: ([Crate] -> [Crate]) -> Procedure -> [Stack] -> [Stack]
runProcedure f (Move count from to) stack = move f count from to stack

sections :: String -> ([String], [String])
sections = bimap (init) (drop 1) . break null . lines

parseStack :: [String] -> [String]
parseStack = map (dropWhile (==' ')) . transpose . parseStrategy row

procedures :: [String] -> [Procedure]
procedures = parseStrategy procedureParser

parseStrategy :: ReadP a -> [String] -> [a]
parseStrategy a = map fst . map (head . readP_to_S a)

-- moves <- procedures . snd . sections <$> sample 5
-- stacks <- parseStack . fst . sections <$> sample 5
-- foldl' (\a x -> runProcedure x a) stacks moves

top :: [Stack] -> String
top = map head

-- Parsers
crate :: ReadP Crate
crate = between (char '[') (char ']') get

emptyStack :: ReadP Crate
emptyStack = do
  count 3 (satisfy isSpace)
  pure ' '

stack :: ReadP Crate
stack = crate <|> emptyStack

row :: ReadP [Crate]
row = manyTill (stack <* (optional (char ' '))) eof

-- Procedure parser

procedureParser :: ReadP Procedure
procedureParser = do
  string "move "
  amount <- read <$> munch1 isDigit
  string " from "
  from <- read <$> munch1 isDigit
  string " to "
  to <- read <$> munch1 isDigit
  pure $ Move amount from to

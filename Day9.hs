module Main where

import Data.Bifunctor (bimap, first, second)
import Data.List
import Data.Char
import Control.Monad.State
import Data.Functor.Identity

import Aoc

main :: IO ()
main = do
  content <- lines <$> input 9
  let moves = parse content
  print $ part1 moves
  print $ part2 moves
  where
    part1 f = length . nub . scanl addPos (0, 0) $ evalState (state headMoves f) Overlapped
    part2 f = length . nub . scanl addPos (0, 0) $ evalState (state headMoves2 f) (replicate 9 Overlapped)
    addPos (x, y) move = (x + (fst $ tailToPos move), y + (snd $ tailToPos move))
    state f = sequence . map f

data TailMovement = TailMove Direction
  | TailStill
  deriving Show

data RelativePosition = Relative Direction
  | Overlapped
  deriving Show

data Direction = NorthWest
  | North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  deriving (Show, Eq, Enum, Bounded)

type RelativeState = RelativePosition

tailToPos :: TailMovement -> (Int, Int)
tailToPos TailStill = (0, 0)
tailToPos (TailMove NorthWest) = (-1, 1)
tailToPos (TailMove North) = (0, 1)
tailToPos (TailMove NorthEast) = (1, 1)
tailToPos (TailMove East) = (1, 0)
tailToPos (TailMove SouthEast) = (1, -1)
tailToPos (TailMove South) = (0, -1)
tailToPos (TailMove SouthWest) = (-1, -1)
tailToPos (TailMove West) = (-1, 0)

compassU :: Direction -> [Direction]
compassU a = go 4 ((fromEnum a + 2) `mod` 8)
  where
    go :: Int -> Int -> [Direction]
    go 0 n = [toEnum n]
    go i n = toEnum n : go (i - 1) ((n + 1) `mod` 8)

compassOpposite :: Direction -> Direction
compassOpposite = toEnum . flip mod 8 . (+4) . fromEnum

side :: Direction -> [Direction]
side a = [toEnum ((fromEnum a - 1) `mod` 8), a, toEnum ((fromEnum a + 1) `mod` 8)]

tailMove :: RelativePosition -> Direction -> (TailMovement, RelativePosition)
tailMove Overlapped b = (TailStill, Relative b)
tailMove (Relative a) b
  | even $ fromEnum b = if a `elem` (compassU $ compassOpposite b)
                          then (TailMove . moveDiagLeft $ oppU b , dleft $ oppU b)
                          else (TailStill, diagRelLeft . side . compassOpposite $ b)
  | a `elem` compassSide = (TailMove a, Relative b)
  | otherwise = (TailStill, left $ compassU b)
  where
    oppU = compassU . compassOpposite
    moveDiagLeft (x:xs) = if x == a then toEnum ((fromEnum a + 1) `mod` 8) else moveDiagMid xs
    moveDiagMid (x:y:z:xs) = if a `elem` [x, y, z] then y else moveDiagRight xs
    moveDiagRight (x:[]) = toEnum ((fromEnum a - 1) `mod` 8)
    diagRelLeft (x:xs) = if x == a then Relative . toEnum $ ((fromEnum a - 2) `mod` 8) else diagRelMid xs
    diagRelMid (x:xs) = if x == a then Overlapped else diagRelRight xs
    diagRelRight (x:[]) = Relative . toEnum $ (fromEnum a + 2) `mod` 8
    compassSide = [toEnum (number - 1), b, toEnum ((number + 1) `mod` 8)]
    number = fromEnum b
    dleft (x:xs) = if x == a then Relative . toEnum $ (fromEnum a + 1) `mod` 8 else dmiddle xs
    dmiddle (x:y:z:xs) = if a `elem` [x, y, z] then Relative a else dright xs
    dright (x:[]) = Relative . toEnum $ (fromEnum a - 1) `mod` 8
    left (x:y:xs) = if x == a || y == a then Relative . toEnum $ (fromEnum a - 1) `mod` 8 else middle xs
    middle (x:xs) = if x == a then Overlapped else right xs
    right (x:y:[]) = Relative . toEnum $ (fromEnum a + 1) `mod` 8

headMoves :: Direction -> StateT RelativeState Identity TailMovement
headMoves dir = do
  rel <- get

  let (tail, newRel) = tailMove rel dir
  put newRel

  pure tail

headMoves2 :: Direction -> StateT [RelativeState] Identity TailMovement
headMoves2 dir = do
  rel <- get

  let hd = head rel
  let firstMove = tailMove hd dir
  let snake = scanl (\b a -> bwah (fst b) a) firstMove (tail rel)

  put . map snd $ snake

  pure . fst . last $ snake
  where
    bwah :: TailMovement -> RelativePosition -> (TailMovement, RelativePosition)
    bwah (TailMove b) a = tailMove a b
    bwah TailStill a = (TailStill, a)

parse :: [String] -> [Direction]
parse = concatMap (uncurry movement . bimap direction (read . drop 1) . break isSpace)
  where
    movement :: Direction -> Int -> [Direction]
    movement dir count = replicate count dir
    direction "R" = East
    direction "L" = West
    direction "U" = North
    direction "D" = South
    direction _ = error "Malformed input"

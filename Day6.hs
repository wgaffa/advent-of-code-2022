module Main where

import Data.Bifunctor (bimap, first, second)
import Data.List
import Data.Char
import qualified Data.Set as S

import Aoc

main :: IO ()
main = do
  content <- input 6
  let run = Aoc.run content
  run 1282 $ calibrate 4
  run 3513 $ calibrate 14

calibrate :: Int -> String -> Int
calibrate packetSize = go 0
  where
    go i xs = case packetValidation (take packetSize xs) of
      (Calibrate c) -> go (i + c) (drop c xs)
      Ok -> i + packetSize

data Marker = Calibrate Int | Ok
  deriving (Show)

packetValidation :: String -> Marker
packetValidation = go 0 []
  where
    go i _ [] = Ok
    go i set (x:xs) = case elemIndex x set of
      (Just c) -> Calibrate (c + 1)
      Nothing -> go (i + 1) (set ++ [x]) xs

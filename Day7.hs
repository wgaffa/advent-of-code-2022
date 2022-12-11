module Main where

import Data.Bifunctor (bimap, first, second)
import Data.Function
import Data.List
import Data.Char
import Data.Maybe
import Data.Functor.Identity
import qualified Data.Map as M

import Control.Monad.State

import Control.Applicative ((<|>))
import Text.ParserCombinators.ReadP hiding (get)

import Aoc

main :: IO ()
main = do
  content <- input 7
  let run = Aoc.run content
  run 1243729 strategy
  run 4443914 $ \s -> do
    let fstats = filemap s
    let smallestDir = used fstats >>= return . calculation >>= return . (\x -> M.filter(>=x) fstats) >>= return . snd . minimumBy (compare `on` snd) . M.toAscList
    fromJust smallestDir
  where
    needed = 30000000
    total = 70000000
    calculation x = needed - (total - x)
    used = M.lookup "/"
    strategy = sum . map snd . M.toAscList . M.filter (<= 100000) . filemap
    filemap = snd . snd . runIdentity . state
    state = flip runStateT ([], M.empty) . commands
    commands = sequence_ . concatMap (map fst) . parse
    parse = map (readP_to_S mainParse) . lines

data Dir = Dir String [Dir] | File String

data Direction = Root | Parent | Path String
  deriving Show

type CurrentPath = [String]

type WorkingDirState a = StateT (CurrentPath, M.Map String Int) Identity a

goDir :: Direction -> WorkingDirState ()
goDir Root = do
  (_, map) <- get
  put (["/"], map)
goDir Parent = do
  (cwd, map) <- get
  put (drop 1 cwd, map)
goDir (Path s) = do
  (cwd, map) <- get
  put (s:cwd, map)

fStat :: Int -> String -> WorkingDirState ()
fStat a _ = do
  (cwd, map) <- get
  let dirs = scanr1 (flip pathJoin) cwd
  let new = foldl' (\m k -> M.alter (f a) k m) map dirs
  -- let new' = M.alter (f a) (head dirs) new
  put (cwd, new)
  where
    f x Nothing = Just x
    f x (Just old) = Just $ x + old

test :: Int -> Maybe Int -> Maybe Int
test a Nothing = Just a
test a (Just old) = Just $ old + a

getCwd :: WorkingDirState String
getCwd = do
  (cwd, map) <- get
  pure . drop 1 . intercalate "/" . reverse $ cwd

-- maximumBy (compare `on` snd) . M.toAscList . M.filter (< 100000) . snd . snd . runIdentity . flip runStateT ([], M.empty) . sequence_ . concatMap (map fst)
pathJoin :: String -> String -> String
pathJoin xs ys'@(y:ys)
  | last xs == '/' || y == '/' = xs ++ ys'
  | otherwise = xs ++ "/" ++ ys'

-- Parsers

fileName :: Char -> Bool
fileName x = isAlpha x || x == '.'

dirName :: Char -> Bool
dirName x = isAlpha x || x `elem` "./"

parseCd :: ReadP (WorkingDirState ())
parseCd = do
  string "$ cd "
  dir <- munch1 dirName
  case dir of
    "/" -> pure $ goDir Root
    ".." -> pure $ goDir Parent
    s -> pure $ goDir (Path s)

parseEntry :: ReadP (WorkingDirState ())
parseEntry = do
  size <- read <$> munch1 isDigit
  skipSpaces
  name <- munch1 fileName
  pure $ fStat size name

mainParse :: ReadP (WorkingDirState ())
mainParse = parseCd <|> parseEntry

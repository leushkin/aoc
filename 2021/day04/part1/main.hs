{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment ( getArgs )
import Data.List ( groupBy )
import Data.Text ( strip, pack, unpack )
import Numeric ( readInt )
import Distribution.Simple.Utils ( safeHead )
import Data.List

type Row a = [a]
type Board a = [Row a]

boardsLength = 5

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                "" -> []
                s' -> w : wordsWhen p s''
                      where (w, s'') = break p s'

wordsToInt :: String -> [Int]
wordsToInt = map read . words

dropIfHeadIs :: String -> [String] -> [String]
dropIfHeadIs str list = if head list == str then tail list else list

colsToRows :: [[a]] -> [[a]]
colsToRows board = map pickColData boardIndices
  where
    pickColData index = [x !! index | x <- board]
    boardIndices = [0 .. (boardsLength - 1)]

checkCols :: Row Int -> Board Int -> Bool
checkCols numbers board = checkRows numbers $ colsToRows board

checkRows :: Row Int -> Board Int -> Bool
checkRows numbers board = any and mapped
  where
    mapped = map (map (`elem` numbers)) board

checkWin :: [Int] -> Board Int -> Bool
checkWin numbers board = cols || rows
  where
    cols = checkCols numbers board
    rows = checkRows numbers board

parseInput :: String -> (Row Int, [Board Int])
parseInput input = (numbers, boards)
  where
    (numbersInput : boardsInput) = lines input
    numbers = map read . wordsWhen (==',') $ numbersInput
    boards =
      map (map wordsToInt . dropIfHeadIs "")
      $ groupBy (\x y -> y /= "")
      $ tail boardsInput

findWinBoard :: Row Int -> [Board Int] -> [Board Int]
findWinBoard numbers = filter (checkWin numbers)

iterativeFindWinBoard :: Row Int -> Row Int -> [Board Int] -> (Row Int, Board Int)
iterativeFindWinBoard _ _ [] = error "nothing found"
iterativeFindWinBoard _ [] _ = error "nothing found"
iterativeFindWinBoard tried (x:xs) boards =
  case board of
    Just x -> (tried, x)
    Nothing -> iterativeFindWinBoard (tried <> [x]) xs boards
  where
    board = safeHead $ findWinBoard tried boards

solution :: String -> Int
solution input = last xs * sum unmarked
  where
    unmarked = concat board \\ xs
    (xs, board) = iterativeFindWinBoard [] numbers boards
    (numbers, boards) = parseInput input

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args
  print . solution $ contents
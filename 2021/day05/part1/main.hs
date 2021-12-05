module Main where

import System.Environment ( getArgs )
import Data.Map ( empty, insertWith, elems )

type Point = (Int, Int)
type Line = (Point, Point)

tuple2 :: [a] -> (a, a)
tuple2 list = (a, b)
  where [a, b] = list

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                "" -> []
                s' -> w : wordsWhen p s''
                      where (w, s'') = break p s'

stringToPoint :: String -> Point
stringToPoint str = tuple2 . map read $ wordsWhen (==',') str

stringToLine :: String -> Line
stringToLine = tuple2 . map stringToPoint . filter (/="->") . words

rangeFromLine :: Line -> [Point]
rangeFromLine line@(p1, p2)
  | x1 == x2 = zip (repeat x1) (createRange y1 y2)
  | y1 == y2 = zip (createRange x1 x2) (repeat y1)
  | otherwise = []
  where
    createRange a b = if a >= b then [b .. a] else [a .. b]
    x1 = fst p1
    y1 = snd p1
    x2 = fst p2
    y2 = snd p2

solution :: [Line] -> Int
solution lines = length . filter (>=2) . elems . foldl (\acc x -> insertWith (+) x 1 acc) empty $ ranges
  where
    ranges = concatMap rangeFromLine lines

parseInput :: String -> [Line]
parseInput = map stringToLine . lines

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args
  print $ solution . parseInput $ contents

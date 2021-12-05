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

isLinear :: Line -> Point -> Bool
isLinear (p1, p2) temp@(x, y)
  | temp == p2 = True
  | temp < p2 = False
  | otherwise = isLinear (p1, p2) (x - 1, y + 1)

createLinearRange :: Line -> Point -> [Point] -> [Point]
createLinearRange (p1, p2) temp@(x, y) range
  | temp <= p1 = temp : range
  | otherwise = createLinearRange (p1, p2) (x - 1, y + 1) (temp : range)

rangeFromLine :: Line -> [Point]
rangeFromLine line@(p1, p2)
  | x1 == x2 = zip (repeat x1) (createRange y1 y2)
  | y1 == y2 = zip (createRange x1 x2) (repeat y1)
  | x1 == y1 && x2 == y2 = zip [x1 .. x2] [y1 .. y2]
  | isLinear (minP, maxP) maxP = createLinearRange (minP, maxP) maxP []
  | otherwise = []
  where
    createRange a b = if a >= b then [b .. a] else [a .. b]
    x1 = fst p1
    y1 = snd p1
    x2 = fst p2
    y2 = snd p2
    maxP = max p1 p2
    minP = min p1 p2

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

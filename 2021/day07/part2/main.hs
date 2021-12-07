module Main where

import System.Environment ( getArgs )
import Data.List ( sort )

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                "" -> []
                s' -> w : wordsWhen p s''
                      where (w, s'') = break p s'

parseInput :: String -> [Int]
parseInput = map read . wordsWhen (==',')

middle :: [Int] -> Int
middle list = list !! (length list `div` 2)

solution :: String -> Int
solution input = foldl (\acc d -> acc + ((d * (d + 1)) `div` 2)) 0 $ map (\x -> abs (x - median)) x
  where
    x = parseInput input
    median = middle . sort $ x

main :: IO ()
main = do
 args <- getArgs
 contents <- readFile $ head args
 print . solution $ contents
module Main where

import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Numeric (readInt)
import Data.Char

binaryStringToInt :: String -> Maybe Int
binaryStringToInt = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

findOneWith :: (Int -> Int -> Bool) -> [String] -> Int -> String
findOneWith _ (x : []) _ = x
findOneWith f input index = if f (length zeros) (length ones)
                          then findOneWith f zeros (index + 1)
                          else findOneWith f ones (index + 1)
  where
    zeros = filter (\x -> (x !! index) == '0') input
    ones = filter (\x -> (x !! index) == '1') input

oxygenRating :: [String] -> Int -> String
oxygenRating = findOneWith (>)

co2Scrubber :: [String] -> Int -> String
co2Scrubber = findOneWith (<=)

solution :: [String] -> Maybe Int
solution input = (*) <$> oxygen <*> co2
  where
    oxygen = binaryStringToInt $ oxygenRating input 0
    co2 = binaryStringToInt $ co2Scrubber input 0

main = do
  args <- getArgs
  contents <- readFile $ head args
  print $ solution $ lines contents
module Main where

import System.Environment (getArgs)
import Data.Maybe (listToMaybe)
import Numeric (readInt)
import Data.Char

inverseBinary :: Int -> Int
inverseBinary 0 = 1
inverseBinary 1 = 0
inverseBinary _ = error "its not binary number u dumbass"

binaryStringToInt :: String -> Maybe Int
binaryStringToInt = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

clamp :: Ord a => a -> a -> a -> a
clamp low high = min high .max low

matrixToDigits :: [[Char]] -> [[Int]]
matrixToDigits = map $ map digitToInt

zeroToMunisOne :: [[Int]] -> [[Int]]
zeroToMunisOne = map $ map (\x -> if x == 0 then -1 else x)

evalMatrix :: [[Int]] -> [Int]
evalMatrix = foldl (zipWith (+)) (repeat 0)

mapTuple2 :: (a -> b) -> (a, a) -> (b, b)
mapTuple2 f (x, y) = (f x, f y)

solution :: [String] -> Maybe Int
solution input = (*) <$> gamma <*> epsilon
  where
    (gamma, epsilon) =
      mapTuple2 (binaryStringToInt . map intToDigit)
      . (\x -> (x, map inverseBinary x))
      . map (clamp 0 1)
      . evalMatrix
      . zeroToMunisOne
      . matrixToDigits
      $ input

main = do
  args <- getArgs
  contents <- readFile $ head args
  print . solution . lines $ contents
module Main where

import Text.Printf
import Data.List

hasPair :: String -> Bool
hasPair str = case str of
  [] -> False
  [x] -> False
  (x:y:xs) -> if isInfixOf [x, y] xs then True else hasPair $ y : xs

hasSurround :: String -> Bool
hasSurround str = case str of
  (x:y:z:xs) -> if (x == z) then True else hasSurround $ y : z : xs
  _ -> False

isNice :: String -> Bool
isNice s = hasPair s && hasSurround s

solution :: [String] -> Int
solution = foldl1 (+) . map (fromEnum . isNice)

main = do
  input <- lines <$> readFile "input.txt"
  printf "%d" $ solution input
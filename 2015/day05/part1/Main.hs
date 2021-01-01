module Main where

import Text.Printf
import Data.List
import Control.Applicative

-- Pretty stupid function but I'm totally ok with it
dedup :: (Eq a) => [a] -> [a]
dedup xs = case xs of
  [] -> []
  [x] -> [x]
  (x:y:xs) -> if x == y then y : dedup xs else x : (dedup $ y : xs)

hasBadSubstr :: String -> Bool
hasBadSubstr xs = case xs of
  [] -> False
  [x] -> False
  (x:y:xs) -> if (elem [x, y] bad) then True else hasBadSubstr $ y : xs
  where
    bad = ["ab", "cd", "pq", "xy"]

isVowel :: Char -> Bool
isVowel c = elem c ['a', 'e', 'i', 'o', 'u']

has3Vowels :: String -> Bool
has3Vowels = (>= 3) . foldl (+) 0 . map (fromEnum . isVowel)

hasDoubles :: (Ord a) => [a] -> Bool
hasDoubles str = length str /= (length . dedup $ str)

isNice :: String -> Bool
isNice s = has3Vowels s && hasDoubles s && (not $ hasBadSubstr s)

solution :: [String] -> Int
solution = foldl (+) 0 . map (fromEnum . isNice)

main = do
  input <- lines <$> readFile "input.txt"
  printf "%d" $ solution input
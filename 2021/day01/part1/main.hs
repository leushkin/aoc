module Main where

import System.Environment (getArgs)

solution :: [Int] -> Int
solution a = foldl (+) 0
  $ map (fromEnum . (> 0))
  $ tail
  $ map (uncurry (-))
  $ zip a (0:a)

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args
  print . solution . map readInt . words $ contents
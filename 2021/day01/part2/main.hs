module Main where

import System.Environment (getArgs)

solution :: [Int] -> Int
solution list = foldl (+) 0
  $ map (fromEnum . (> 0))
  $ tail
  $ map (uncurry (-))
  $ zip a (0:a)
  where a = adapter list

adapter :: [Int] -> [Int]
adapter a = drop 2 $ map unplus $ zip list'' $ map unplus $ zip a list'
  where
    unplus = uncurry (+)
    list' = (0:a)
    list'' = (0:0:a)

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args
  print . solution . map readInt . words $ contents
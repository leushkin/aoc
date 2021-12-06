module Main where

import System.Environment ( getArgs )

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                "" -> []
                s' -> w : wordsWhen p s''
                      where (w, s'') = break p s'

process :: Int -> Int
process timer
  | timer == 0 = 6
  | otherwise =  timer - 1

parseInput :: String -> [Int]
parseInput input = map read $ wordsWhen (==',') input

runOneDay :: [Int] -> ([Int], [Int])
runOneDay state = (newFishes, nextDay)
  where
    nextDay = map process state
    newFishes = map (const 8) . filter (==0) $ nextDay

foldHelper :: ([Int], [Int]) -> Int -> ([Int], [Int])
foldHelper (newFishes, previousDay) day = (nextDayFishes, nextDay <> newFishes)
  where (nextDayFishes, nextDay) = runOneDay previousDay 

solution :: String -> Int
solution input = length . snd $ foldl foldHelper ([], initialState) days
  where
    initialState = parseInput input
    days = [0 .. 79]

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args
  print . solution $ contents
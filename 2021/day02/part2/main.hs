module Main where

import System.Environment (getArgs)

data Command = Forward Int | Down Int | Up Int
  deriving Show

parseCommand :: String -> Command
parseCommand input = case command of
                      "forward" -> Forward value'
                      "down" -> Down value'
                      "up" -> Up value'
                      _ -> error $ "unexpected input: " <> command
  where
    (command:value:_) = words input
    value' = (read value) :: Int

parseCommands :: [String] -> [Command]
parseCommands = map parseCommand

solution :: [Command] -> Int
solution commands = position * depth
  where
    (position, depth) = helper commands 0 0 0
    helper (x:xs) aim pos depth = case x of
                                  Down x -> helper xs (aim + x) pos depth
                                  Up x -> helper xs (aim - x) pos depth
                                  Forward x -> helper xs aim (pos + x) (depth + x * aim)
    helper [] aim pos depth = (pos, depth)

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args
  print . solution . parseCommands . lines $ contents
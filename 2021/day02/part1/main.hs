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
solution commands = forwards * (downs - ups)
  where
    ups = foldl (+) 0 [x | Up x <- commands]
    downs = foldl (+) 0 [x | Down x <- commands]
    forwards = foldl (+) 0 [x | Forward x <- commands]

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args
  print . solution . parseCommands . lines $ contents
import System.Environment( getArgs )
import System.Exit( die )
import Text.Read( readMaybe )

import qualified Auto

main :: IO ()
main = do
  args <- getArgs
  if length args == 1 then
    do
      input <- readFile $ head args
      putStrLn $ handle input
  else
    die "Usage: RunAuto filename"

range :: (Enum t, Num t) => t -> [t]
range n = [1..n]

parseMaxState :: String -> Maybe Int
parseMaxState input = do
  maxState <- readMaybe input
  if maxState < 1 then Nothing else return maxState

parseStatesList :: Int -> String -> Maybe [Int]
parseStatesList maxState input = do
  statesList <- readMaybe input
  if all (\x -> 1 <= x && x <= maxState) statesList then return statesList else Nothing

parseTransitions :: Int -> [String] -> Maybe [String]
parseTransitions maxState = Just

parseWord :: [String] -> Maybe String
parseWord [] = Nothing
parseWord lst = Just (last lst)

parse :: [String] -> Maybe ([Int], [Int], [Int], [String], String)
parse (s:is:as:rest) = do
  maxState <- parseMaxState s
  let states = [1..maxState]
  initStates <- parseStatesList maxState is
  ackStates <- parseStatesList maxState as
  transitions <- parseTransitions maxState $ init rest
  word <- parseWord rest
  return (states, initStates, ackStates, transitions, word)
parse _ = Nothing

handle :: String -> String
handle input = show $ parse (getLines input)
  where
    getLines = (filter (not . null) .) lines

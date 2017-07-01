import Control.Monad (when)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

import qualified Auto

main :: IO ()
main = do
  args <- getArgs
  if length args == 1
    then do
      input <- readFile $ head args
      putStrLn $ handle input
    else die "Usage: RunAuto filename"

handle :: String -> String
handle input = maybe "BAD INPUT" show (parse (getLines input))
  where
    getLines = (filter (not . null) .) lines

parse :: [String] -> Maybe Bool
parse (s:is:as:rest) = do
  maxState <- parseMaxState s
  let states = [1 .. maxState]
  initStates <- parseStatesList maxState is
  ackStates <- parseStatesList maxState as
  Control.Monad.when (null rest) Nothing
  transitions <- parseTransitions maxState $ init rest
  word <- parseLastList rest
  return (Auto.accepts (Auto.fromLists states initStates ackStates transitions) word)
parse _ = Nothing

parseMaxState :: String -> Maybe Int
parseMaxState input = do
  maxState <- readMaybe input
  if maxState < 0
    then Nothing
    else return maxState

parseStatesList :: Int -> String -> Maybe [Int]
parseStatesList maxState input = do
  statesList <- readMaybe input
  if all (stateInRange maxState) statesList
    then return statesList
    else Nothing

parseTransitions :: Int -> [String] -> Maybe [(Int, Char, [Int])]
parseTransitions maxState input = concat <$> mapM (parseTransition maxState . words) input

parseTransition :: Int -> [String] -> Maybe [(Int, Char, [Int])]
parseTransition maxState (s:zs:ss) = do
  state <- parseState maxState s
  characters <- parseWord zs
  states <- mapM readMaybe ss
  return [(state, character, states) | character <- characters]
parseTransition _ _ = Nothing

parseState :: Int -> String -> Maybe Int
parseState maxState input = do
  state <- readMaybe input
  if stateInRange maxState state
    then return state
    else Nothing

parseLastList :: [String] -> Maybe String
parseLastList [] = Nothing
parseLastList lst = parseWord (last lst)

parseWord :: String -> Maybe String
parseWord input =
  if all charInRange input
    then return input
    else Nothing

stateInRange
  :: (Num a, Ord a)
  => a -> a -> Bool
stateInRange n x = 1 <= x && x <= n

charInRange :: Char -> Bool
charInRange = flip elem ['A' .. 'Z']


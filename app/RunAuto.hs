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

stateInRange :: (Num a, Ord a) => a -> a -> Bool
stateInRange n x = 1 <= x && x <= n

parseMaxState :: String -> Maybe Int
parseMaxState input = do
  maxState <- readMaybe input
  if maxState < 1 then Nothing else return maxState

parseStatesList :: Int -> String -> Maybe [Int]
parseStatesList maxState input = do
  statesList <- readMaybe input
  if all (stateInRange maxState) statesList then return statesList else Nothing

parseState :: Int -> String -> Maybe Int
parseState maxState input = do
  state <- readMaybe input
  if stateInRange maxState state then return state else Nothing

charInRange :: Char -> Bool
charInRange = flip elem ['A' .. 'Z']

parseWord :: String -> Maybe String
parseWord input = if all charInRange input then return input else Nothing

parseTransition :: Int -> [String] -> Maybe [(Int, Char, [Int])]
parseTransition maxState (s:zs:ss) = do
  state <- parseState maxState s
  characters <- parseWord zs
  states <- mapM readMaybe ss
  return [(state, character, states) | character <- characters]
parseTransition _ _ = Nothing

parseTransitions :: Int -> [String] -> Maybe [(Int, Char, [Int])]
parseTransitions maxState input = concat <$> mapM (parseTransition maxState . words) input

parseLastList :: [String] -> Maybe String
parseLastList [] = Nothing
parseLastList lst = parseWord (last lst)

parse :: [String] -> Maybe ([Int], [Int], [Int], [(Int, Char, [Int])], String)
parse (s:is:as:rest) = do
  maxState <- parseMaxState s
  let states = [1..maxState]
  initStates <- parseStatesList maxState is
  ackStates <- parseStatesList maxState as
  transitions <- parseTransitions maxState $ init rest
  word <- parseLastList rest
  return (states, initStates, ackStates, transitions, word)
parse _ = Nothing

handle :: String -> String
handle input = show $ parse (getLines input)
  where
    getLines = (filter (not . null) .) lines

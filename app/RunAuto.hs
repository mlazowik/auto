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

range n = [1..n]

parseStates input = range <$> (readMaybe input :: Maybe Int)

parseStatesList input = readMaybe input :: Maybe [Int]

parseTransitions = id

parse (states:initStates:ackStates:rest) = (parseStates states, parseStatesList initStates, parseStatesList ackStates,
 parseTransitions $ tail $ reverse rest, last rest)

handle input = show $ parse (getLines input)
  where
    getLines = (filter (not . null) .) lines

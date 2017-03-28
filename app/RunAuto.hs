import System.Environment( getArgs )
import System.Exit( die )

import qualified Auto

main :: IO ()
main = do
  args <- getArgs
  if length args == 1 then
    do
      content <- readFile $ head args
      putStrLn $ handle content
  else
    die "Usage: RunAuto filename"

handle = id
module Main where
import Interpreter
import System.Environment
import Prelude

main :: IO ()
main = do
  args <- getArgs
  case args of
    [f] -> interpretFile f
    [] -> interpretInput
    _ -> putStrLn "Invalid arguments."


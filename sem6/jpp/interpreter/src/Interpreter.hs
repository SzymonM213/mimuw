module Interpreter where
import Prelude
import Parser.Par
import TypeChecker.TypeChecker
import Evaluator.Evaluator
import System.IO
import System.Exit

interpretFile :: String -> IO ()
interpretFile path = do
    content <- readFile path
    interpret content

interpretInput :: IO ()
interpretInput = do
    content <- getContents
    interpret content

interpret :: String -> IO ()
interpret content = do
    case pProgram (myLexer content) of
        Left err -> hPutStrLn stderr $ "Parse error: " ++ show err
        Right tree -> do
            case checkTypes tree of
                Left err -> hPutStrLn stderr $ "Type checking error: " ++ show err
                Right _ -> do
                    result <- eval tree
                    case result of
                        Left err -> hPutStrLn stderr $ "Runtime error: " ++ show err
                        Right v -> do
                            putStrLn $ "Program executed with value: " ++ show v
                            exitSuccess

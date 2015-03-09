import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsCPP
import LexCPP
import ParCPP
import ErrM

import TypeChecker
import Compiler

-- driver

check :: String -> String -> IO ()
check s file = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure
            Ok  tree -> case typecheck tree of
                          Bad err -> do
                            putStrLn "TYPE ERROR"
                            putStrLn err
                            exitFailure
                          Ok _ -> compile tree file

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do
              d <- readFile file
              check d file
            _      -> do putStrLn "Usage: lab3 <SourceFile>"
                         exitFailure

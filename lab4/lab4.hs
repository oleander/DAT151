import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsCPP
import LexCPP
import PrintCPP
import ParCPP
import ErrM

import Interpreter

-- driver

check :: String -> Bool -> IO ()
check s c = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure
            Ok  tree -> case interpret tree c of
                          Bad err -> do
                            putStrLn "ERROR:"
                            putStrLn err
                            exitFailure
                          Ok (VInt e) -> print e
                          Ok e -> print $ "ERROR: wrong return type " ++ show e

main :: IO ()
main = do args <- getArgs
          case args of
            [file]       -> readFile file >>= (\n -> check n True)
            [file, "-n"] -> readFile file >>= (\n -> check n True)
            [file, "-v"] -> readFile file >>= (\n -> check n False)
            _      -> do putStrLn "Usage: lab2 <SourceFile>"
                         exitFailure

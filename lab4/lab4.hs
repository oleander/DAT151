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
                          Ok (EInt n) -> print n
                          Ok e -> print $ "ERROR: wrong return type for" ++ printTree e

main :: IO ()
main = do args <- getArgs
          case args of
            [file]       -> readFile file >>= (\n -> check n True)
            ["-n", file] -> readFile file >>= (\n -> check n False)
            ["-v", file] -> readFile file >>= (\n -> check n True)
            _      -> do putStrLn "Usage: lab4 <SourceFile>"
                         exitFailure

module Compiler where

import AbsCPP
import PrintCPP
import ErrM

import Control.Monad
import qualified Data.Map as Map
import Data.IORef
import Data.Maybe

import Data.List (nub)

type Block = Map.Map String Integer
type Scope = [Block]
type Fun = (Type, [Arg])
type FunScope = Map.Map Id Fun
data Env = Env { 
  labels :: Integer,
  scope :: Scope,
  functionScope :: FunScope,
  addrCounter :: Integer
} deriving(Show)

emptyScope :: Scope
emptyScope = [Map.empty]

emptyFunctionScope :: FunScope
emptyFunctionScope = Map.empty

newBlock :: Env -> Env
newBlock env@Env { scope = scope } = 
  env { scope = Map.empty : scope }

removeBlock :: Env -> Env
removeBlock env@Env { scope = scope } = 
  env { scope = tail scope }

lookupAddr :: String -> Env -> Err Integer
lookupAddr i env@Env{ scope = scope' } =
  case scope' of
    []    -> fail $ show i ++ " wasn't found"
    (f:e) ->
      case Map.lookup i f of
        Just val -> return val
        Nothing -> lookupAddr i env { scope = e }

lookupFun :: Id -> Env -> Err Fun
lookupFun i env@Env { functionScope = scope } =
  case Map.lookup i scope of
    Just val -> return val
    Nothing -> fail $ show i ++  " not found"

emptyEnv :: Env
emptyEnv = Env { 
  labels = 0, 
  scope = emptyScope,
  functionScope = emptyFunctionScope,
  addrCounter = 0
}

newLabel :: Env -> (String, Env)
newLabel env@Env{ labels = l } = (label, env')
  where 
    label = "Label" ++ show l
    env'  = env { labels = (l + 1) }

extend :: Id -> Env -> Err Env
extend (Id i) env@Env{ addrCounter = counter, scope = (block:s) } = 
  if Map.member i block
  then fail $ show i ++ " already declared"
  else return $ env { addrCounter = counter + 1, scope = scope' }
  where
    scope' = Map.insert i counter block : s

emit :: String -> IO()
emit = print

compile :: Program -> IO ()
compile program = undefined

compileExp :: Exp -> IO ()
compileExp = undefined

compileStm :: Stm -> IO ()
compileStm = undefined

compileDef :: Def -> IO ()
compileDef = undefined
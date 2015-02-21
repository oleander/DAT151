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
  labelCounter :: Integer,
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

-- TODO: Free space by decrementing addrCounter
removeBlock :: Env -> Env
removeBlock env@Env { scope = scope } = 
  env { scope = tail scope }

lookupAddr :: Id -> Env -> Integer
lookupAddr (Id i) env@Env{ scope = scope' } =
  case scope' of
    []    -> error $ show i ++ " wasn't found"
    (f:e) ->
      case Map.lookup i f of
        Just val -> val
        Nothing -> lookupAddr (Id i) env { scope = e }

lookupFun :: Id -> Env -> Fun
lookupFun i env@Env { functionScope = scope } =
  case Map.lookup i scope of
    Just val -> val
    Nothing -> error $ show i ++ " not found"

emptyEnv :: Env
emptyEnv = Env {
  labelCounter  = 0,
  addrCounter   = 0,
  scope         = emptyScope,
  functionScope = emptyFunctionScope
}

newLabel :: Env -> (String, Env)
newLabel env@Env{ labelCounter = l } = (label, env')
  where 
    label = "Label" ++ show l
    env'  = env { labelCounter = l + 1 }

extend :: Id -> Env -> Env
extend (Id i) env@Env{ addrCounter = counter, scope = (block:s) } = 
  if Map.member i block
  then error $ show i ++ " already declared"
  -- Always increment counter by one as we don't need doubles
  else env { addrCounter = counter + 1, scope = scope' }
  where
    scope' = Map.insert i counter block : s

extendFun :: Def -> Env -> Env
extendFun (DFun t i args _) env@Env{ functionScope = functionScope } = 
  if Map.member i functionScope
  then error $ show i ++ " already declared"
  else env { functionScope = functionScope' }
  where
    functionScope' = Map.insert i (t, args) functionScope

compile :: Program -> IO ()
compile program = undefined

compileExp :: Exp -> Env -> IO ()
compileExp (EInt i) env = emit $ "ldc " ++ show i
compileExp (EPlus a b) env = do
  compileExp a env
  compileExp b env
  emit "iadd"
compileExp (EDiv a b) env = do
  compileExp a env
  compileExp b env
  emit "idiv"
compileExp (EId i) env = emit $ "iload " ++ show i'
  where i' = lookupAddr i env

compileStms :: [Stm] -> Env -> IO ()
compileStms (stm:stms) env =
  case stm of
    (SDecls t ids) -> compileStms stms env'
      where env' = foldr (\i env -> extend i env) env ids
    (SInit t i exp) -> do
      compileExp exp env
      emit "dup"
      emit $ "istore " ++ show i'
      where i' = lookupAddr i env
    (SExp exp) -> do
      compileExp exp env
      emit "pop"

compileDef :: Def -> Env -> IO ()
compileDef = undefined

emit :: Show a => a -> IO()
emit = print
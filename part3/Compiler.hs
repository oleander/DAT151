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

lookupAddr :: Id -> Env -> Err Integer
lookupAddr (Id i) env@Env{ scope = scope' } =
  case scope' of
    []    -> fail $ show i ++ " wasn't found"
    (f:e) ->
      case Map.lookup i f of
        Just val -> return val
        Nothing -> lookupAddr (Id i) env { scope = e }

lookupFun :: Id -> Env -> Err Fun
lookupFun i env@Env { functionScope = scope } =
  case Map.lookup i scope of
    Just val -> return val
    Nothing -> fail $ show i ++ " not found"

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

extend :: Id -> Env -> Err Env
extend (Id i) env@Env{ addrCounter = counter, scope = (block:s) } = 
  if Map.member i block
  then fail $ show i ++ " already declared"
  -- Always increment counter by one as we don't need doubles
  else return $ env { addrCounter = counter + 1, scope = scope' }
  where
    scope' = Map.insert i counter block : s

extendFun :: Def -> Env -> Err Env
extendFun (DFun t i args _) env@Env{ functionScope = functionScope } = 
  if Map.member i functionScope
  then fail $ show i ++ " already declared"
  else return $ env { functionScope = functionScope' }
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
compileExp (EId i) env = 
  case lookupAddr i env of
    Ok i -> emit $ "iload " ++ show i
    Bad e -> error e
--compileExp (EAss a b) env do
--  compileExp b
--  compileExp a

compileStm :: Stm -> Env -> IO ()
compileStm (SInit t i exp) env = do
  compileExp exp env
  emit "dup"
  case lookupAddr i env of
    Ok i -> emit $ "istore " ++ show i
    Bad e -> error e
-- TODO: Check for type void
compileStm (SExp exp) env = do
  compileExp exp env
  emit "pop"
compileStm (SDecls _ []) env      = return ()
compileStm (SDecls t (i:ids)) env = do
  case extend i env of
    Ok env' -> compileStm (SDecls t ids) env'
    Bad e -> error e

compileDef :: Def -> Env -> IO ()
compileDef = undefined

emit :: Show a => a -> IO()
emit = print
module Compiler where

import AbsCPP
import PrintCPP
import ErrM

import Control.Monad
import qualified Data.Map as Map
import Data.IORef
import Data.Maybe
import Control.Monad.IO.Class
import Data.IORef

import Data.List (nub)

type Block = Map.Map String Integer
type Scope = [Block]
type Fun = (Type, [Arg])
type FunScope = Map.Map Id Fun
type Operator = String
type Counter = Int -> IO Int
data Env = Env { 
  labelCounter :: Integer,
  scope :: Scope,
  functionScope :: FunScope,
  addrCounter :: Integer,
  counter :: IO Counter
}

makeCounter :: IO Counter
makeCounter = do
    r <- newIORef 0
    return (\i -> do modifyIORef r (+i)
                     readIORef r)

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
  functionScope = emptyFunctionScope,
  counter       = makeCounter
}

newLabel :: Env -> IO String
newLabel env@Env{ counter = c } = do
  counter <- c
  i <- counter 1
  return $ "Label" ++ show i

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
compile (PDefs defs) = do
  emit ".class public C"
  emit ".super java/lang/Object"
  emit ".method public <init>()V"
  emit "aload_0"
  emit "invokenonvirtual java/lang/Object/&lt;init>()V"
  emit "return"
  emit ".end method"
  mapM_ (\def -> compileDef def env) defs
  where env = foldr extendFun emptyEnv defs

compileStms :: [Stm] -> Env -> IO ()
compileStms [] _           = return ()
compileStms (stm:stms) env =
  case stm of
    (SDecls t ids) -> do
      compileStms stms env'
      where env' = foldr extend env ids
    (SInit t i exp) -> do
      compileExp exp env
      emit "dup"
      emit $ "istore " ++ show i'
      compileStms stms env
      where i' = lookupAddr i env
    (SExp exp) -> do
      compileExp exp env
      emit "pop"
      compileStms stms env
    (SBlock s) -> do
      compileStms s (newBlock env)
      compileStms stms (removeBlock env)
    (SIfElse exp s1 s2) -> do
      true <- newLabel env
      false <- newLabel env
      compileExp exp env
      emit $ "ifeq " ++ false
      compileStms [s1] env
      emit $ "goto " ++ true
      emit $ false ++ ":"
      compileStms [s2] env
      emit $ true ++ ":"
      compileStms stms env
    (SWhile exp stm) -> do
      test <- newLabel env
      end <- newLabel env
      emit $ test ++ ":"
      compileExp exp env
      emit $ "ifeq " ++ end
      compileStms [stm] env
      emit $ "goto " ++ test
      emit $ end ++ ":"
      compileStms stms env
    (SReturn exp) -> do
      compileExp exp env
      emit "ireturn"

compileDef :: Def -> Env -> IO ()
compileDef (DFun t (Id i) args stms) env = do
  emit $ ".method public static " ++ i ++ "(" ++ args' ++ ")" ++ (mapType t)
  emit ".limit locals 1000" -- TODO: Calculate this
  emit ".limit stack 1000" -- TODO: Calculate this
  compileStms stms env'
  emit ".end method"
  where 
    args' = compressArgs args
    env' = foldr (\(ADecl _ id) env -> extend id env) env args

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
compileExp (EApp (Id i) exps) env = do
  mapM_ (\exp -> compileExp exp env) exps
  emit $ "invokestatic C/" ++ i ++ "(" ++ args' ++ ")" ++ (mapType rType)
  where 
    args' = compressArgs args
    (rType, args) = lookupFun (Id i) env
-- FIXME: Should update pointer
compileExp (EPIncr exp) env = do
  compileExp exp env
  emit "bipush"
  emit "addi"
compileExp (EDouble exp) env = error "not defined for doubles"
compileExp (EOr e1 e2) env = do
  emit "not implemented"
compileExp (EAnd e1 e2) env = do
  emit "not implemented"
compileExp (EMinus e1 e2) env = do
  emit "not implemented"
compileExp (ETimes e1 e2) env = do
  emit "not implemented"
compileExp (EDecr exp) env = do
  emit "not implemented"
compileExp (EIncr exp) env = do
  emit "not implemented"
compileExp ETrue env = do
  emit "not implemented"
compileExp EFalse env = do
  emit "not implemented"
compileExp (ELtEq e1 e2) env = compareExp "if_icmple" e1 e2 env
compileExp (ELt e1 e2) env = compareExp "if_icmplt" e1 e2 env
compileExp (EGtWq e1 e2) env = compareExp "if_icmpge" e1 e2 env
compileExp (EGt e1 e2) env = compareExp "if_icmpgt" e1 e2 env
compileExp (ENEq e1 e2) env = compareExp "if_icmpne" e1 e2 env
compileExp (EEq e1 e2) env = compareExp "if_icmpeq" e1 e2 env

-- int a, int b => II
-- bool a, int b => ZI
compressArgs :: [Arg] -> String
compressArgs [] = ""
compressArgs ((ADecl t _):args) = (mapType t) ++ compressArgs args

mapType :: Type -> String
mapType Type_bool = "Z"
mapType Type_int = "I"
mapType Type_void = "V"
mapType t = error $ "not defined for " ++ show t

compareExp :: Operator -> Exp -> Exp -> Env -> IO ()
compareExp operator e1 e2 env = do
  true <- newLabel env
  emit "bitpush 1"
  compileExp e1 env
  compileExp e2 env
  emit $ (show operator) ++ " " ++ true
  emit "pop"
  emit "bitpush 0"
  emit $ true ++ ":"

emit :: Show a => a -> IO()
emit = print
{-# LANGUAGE RankNTypes #-}
module Interpreter where

import AbsCPP
import PrintCPP
import Debug.Trace
import ErrM

import Control.Monad
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map

data Val = VInt Integer | VExp Exp deriving(Show)

data Env = Env {
  scope :: [Map.Map Ident Val],
  callByName :: Bool
}

interpret :: Program -> Bool -> Err Val
interpret (Prog defs) _ = do
  env <- foldM defToClos emptyEnv defs
  VExp exp <- findIdent (Ident "main") env
  evalExp exp env

valToExp :: Val -> Exp
valToExp (VExp e) = e
valToExp (VInt i) = EInt i

evalExp :: Exp -> Env -> Err Val
evalExp exp env =
  case trace ("==> " ++ printTree exp) exp of
    EVar i -> a
      where a = findIdent i env
    EInt n -> return $ VInt n
    EApp e1 e2 -> do
      -- Look up e1
      val <- evalExp e1 env
      val2 <- evalExp e2 env

      case val of
        VExp (EAbs ident with) -> 
          evalExp (trace ("AFTER " ++ show ident ++ " ===> " ++ printTree e') e') (addBlock env)
          where e' = findAndReplace ident (trace ("BEFORE " ++ printTree with) with) e2 -- (valToExp val2) -- <== The problem
        e                      -> fail $ "can't apply => " ++ show e ++ " on " ++ show e1
    EAbs i e -> return $ VExp exp
    ESub e1 e2 -> binOp (-) e1 e2 env
    EAdd e1 e2 -> 
      binOp (+) e1 e2 env
      where e1' = trace ("e1 " ++ show e1) e1
            e2' = trace ("e2 " ++ show e2) e2

    EIf this a b -> do
      val <- evalExp this env
      case val of
        VInt 1 -> evalExp a env
        VInt 0 -> evalExp b env
        _      -> fail $ printVal val ++ " is not an int"
    ELt e1 e2 -> do
      v1 <- evalExp e1 env
      v2 <- evalExp e2 env
      case (v1, v2) of
        (VInt a, VInt b) -> 
          if a < b then return $ VInt 1
          else          return $ VInt 0
        (a           ,b) -> fail $ "can't compare\n" ++ printVal a ++ "\nwith\n" ++  printVal b

printVal :: Val -> String
printVal (VInt n) = show n
printVal (VExp e) = printTree e

binOp :: (Integer -> Integer -> Integer) -> Exp -> Exp -> Env -> Err Val
binOp op e1 e2 env = do
  v1 <- evalExp e1 env
  v2 <- evalExp e2 env
  case (v1, v2) of
    (VInt a, VInt b) -> return $ VInt (a `op` b)
    (a,           b) -> fail $ "can't  run " ++ " on " ++ show a ++ " with " ++ show b

emptyEnv :: Env
emptyEnv = Env {
  scope      = [Map.empty],
  callByName = True
}

deleteBlock :: Env -> Env
deleteBlock env@Env { scope = scope } = 
  env { scope = tail scope }

addBlock :: Env -> Env
addBlock env@Env { scope = scope } = 
  env { scope = (Map.empty : scope) }

-- Add ident to scope
addIdent :: Ident -> Val -> Env -> Err Env
addIdent i val env@Env{ scope = (scope:rest) } = 
  if Map.member i scope
  then fail $ show i ++ " already declared"
  else return $ env { scope = (Map.insert i val scope) : rest }

-- Find ident in scope
findIdent :: Ident -> Env -> Err Val
findIdent i env@Env{ scope = [] }          = 
  fail $ "could not find " ++ show i
findIdent i env@Env{ scope = (scope:rest)} = 
  case Map.lookup i scope of
    Just val -> return val
    Nothing  -> findIdent i env { scope = rest }

defToClos :: Env -> Def -> Err Env
defToClos env (DDef fun vars exp) = 
  addIdent fun val env
  where val = VExp $ defToClos' vars exp

-- TODO: Rename this
defToClos' :: [Ident] -> Exp -> Exp
defToClos' []         exp        = exp
defToClos' (var:vars) exp        = 
  EAbs var $ (defToClos' vars exp)

findAndReplace :: Ident -> Exp -> Exp -> Exp
findAndReplace i from to =
  case from of
    EVar ident -> 
      if ident == i then to
      else from
    EInt n -> EInt n
    EApp e1 e2 -> EApp (findAndReplace i e1 to) (findAndReplace i e2 to)
    EAbs ident exp -> 
      if ident == i then from
      else EAbs ident (findAndReplace i exp to)
    ESub e1 e2 -> ESub (findAndReplace i e1 to) (findAndReplace i e2 to)
    EAdd e1 e2 -> EAdd (findAndReplace i e1 to) (findAndReplace i e2 to)
    EIf this a b -> EIf this' a' b'
      where this' = findAndReplace i this to
            a'    = findAndReplace i a to
            b'    = findAndReplace i b to
    ELt e1 e2 -> ELt e1' e2'
      where e1' = findAndReplace i e1 to
            e2' = findAndReplace i e2 to
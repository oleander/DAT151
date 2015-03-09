{-# LANGUAGE RankNTypes #-}
module Interpreter where

import AbsCPP
import PrintCPP
import ErrM

import Control.Monad
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map

data Val = VInt Integer | Clos [Ident] Val | VExp Exp deriving(Show)

data Env = Env {
  scope :: [Map.Map Ident Val],
  callByName :: Bool
}

interpret :: Program -> Bool -> Err Val
interpret (Prog defs) _ = do
  env <- foldM defToClos emptyEnv defs
  val <- findIdent (Ident "main") env
  case val of
    Clos [] (VExp exp) -> evalExp exp env
    Clos _ _    -> fail "main doesn't have any args"
    _           -> fail "main is not a function"

findAndReplaceInVal :: Ident -> Val -> Exp -> Val
findAndReplaceInVal ident val replace =
  case val of
    Clos (i:[]) val'    -> 
      if ident == i then val
      else Clos [i] $ findAndReplaceInVal ident val' replace
    Clos [] val' -> Clos [] $ findAndReplaceInVal ident val' replace
    VExp exp            -> VExp $ findAndReplace ident exp replace
    e -> error $ "what to do?" ++ show e

evalExp :: Exp -> Env -> Err Val
evalExp exp env =
  case exp of
    EVar i -> findIdent i env
    EInt n -> return $ VInt n
    EApp e1 e2 -> do
      -- Look up e1
      fun <- evalExp e1 env 
      case fun of
        Clos (ident:[]) val -> return $ findAndReplaceInVal ident val e2
        val              -> fail $ "not sure what to do" ++ show val ++ " => " ++ show e2
    EAbs i e -> return $ VExp exp
    ESub e1 e2 -> binOp (-) e1 e2 env
    EAdd e1 e2 -> binOp (-) e1 e2 env
    EIf this a b -> do
      val <- evalExp this env
      case val of
        VInt 1 -> evalExp a env
        VInt 0 -> evalExp b env
        e      -> fail $ show exp ++ " is not an int"
    ELt e1 e2 -> do
      v1 <- evalExp e1 env
      v2 <- evalExp e2 env
      case (v1, v2) of
        (VInt a, VInt b) -> 
          if a < b then return $ VInt 1
          else          return $ VInt 0
        (a           ,b) -> fail $ "can't compare " ++ show a ++ " with " ++  show b

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
  where val = defToClos' vars exp

-- TODO: Rename this
defToClos' :: [Ident] -> Exp -> Val
defToClos' []         exp        = Clos [] (VExp exp)
defToClos' (var:vars) exp        = 
  Clos [var] $ (defToClos' vars exp)


findAndReplace :: Ident -> Exp -> Exp -> Exp
findAndReplace i from to =
  case from of
    EVar ident -> 
      if ident == i then to
      else from
    EInt n -> EInt n
    EApp e1 e2 -> EApp (findAndReplace i e1 to) (findAndReplace i e1 to)
    EAbs ident exp -> 
      if ident == i then exp -- 
      else findAndReplace i exp to
    ESub e1 e2 -> ESub (findAndReplace i e1 to) (findAndReplace i e1 to)
    EAdd e1 e2 -> EAdd (findAndReplace i e1 to) (findAndReplace i e1 to)
    EIf this a b -> EIf this' a' b'
      where this' = findAndReplace i this to
            a'    = findAndReplace i a to
            b'    = findAndReplace i b to
    ELt e1 e2 -> ELt e1' e2'
      where e1' = findAndReplace i e1 to
            e2' = findAndReplace i e2 to
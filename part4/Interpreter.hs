{-# LANGUAGE RankNTypes #-}
module Interpreter where

import AbsCPP
import PrintCPP
import ErrM

import Control.Monad
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map

data Val = VInt Int | Clos [Ident] Exp | VExp Exp deriving(Show)

--data Program =
--   Prog [Def]
--  deriving (Eq,Ord,Show,Read)

--data Def =
--   DDef Ident [Ident] Exp
--  deriving (Eq,Ord,Show,Read)

data Env = Env {
  scope :: [Map.Map Ident Val],
  callByName :: Bool
}

interpret :: Program -> Bool -> Err Val
interpret (Prog defs) _ = do
  val <- findIdent (Id "main") env
  case val of
    Clos [] exp -> evalExp exp env
    Clos _ _    -> fail "main doesn't have any args"
    _           -> fail "main is not an function"
  where env = foldM defToClos emptyEnv defs

evalExp :: Exp -> Env -> Err Val
evalExp exp env =
  case exp of
    EVar i -> fail $ "not sure what to do with var " ++ show i -- VExp exp -- findIdent i env
    EInt n -> return $ VInt exp
    EApp e1 e2 ->
      case (evalExp e1 env) of
        Clos ident exp -> evalExp $ findAndReplace ident exp e2 $ env'
        _              -> fail $ "can't apply " show e1 
                            ++ " to " ++ show e2
      where env' = addBlock env
    EAbs i exp -> evalExp (Clos i exp) env
    ESub e1 e2 -> binOp (-) e1 e2 env
    EAdd e1 e2 -> binOp (-) e1 e2 env
    EIf this a b ->
      case evalExp this env of
        VInt 1 -> evalExp a env
        VInt 0 -> evalExp b env
        –      -> fail $ show exp ++ " is not an int"
    ELt e1 e2 exp ->
      case (evalExp e1 env, evalExp e2 env) of
        (VInt a, VInt b) -> 
          if a < b then return $ VInt 1
          else          return $ VInt 0
        (a           ,b) -> fail $ "can't compare " ++ show a ++ " with " ++  show b

binOp :: (a -> a -> a) -> Exp -> Exp -> Env -> Err Val
binOp op e1 e1 env =
  case (evalExp e1 env, evalExp e2 env) of
    (VInt a, VInt b) -> return $ VInt a `op` b
    (a,           b) -> fail $ "can't  run " ++ show op ++ " on " ++ show a ++ " with " ++ show b

emptyEnv :: Env
emptyEnv = Env {
  scope      = [Map.empty],
  callByName = True
}

deleteBlock :: Env
deleteBlock = tail

addBlock :: Env -> Env
addBlock env@Env { scope = scope } = 
  env { scope : Map.empty }

-- Add ident to scope
addIdent :: Ident -> Val -> Env -> Err Env
addIdent i exp env@Env{ scope = (scope:rest) } = 
  if Map.member i scope
  then fail $ show i ++ " already declared"
  else return env { scope = (Map.insert i scope) : rest }

-- Find ident in scope
findIdent :: Ident -> Env -> Err Val
findIdent i env@Env{ scope = [] }          = 
  fail $ "could not find " ++ show i
findIdent i env@Env{ scope = (scope:rest)} = 
  case Map.lookup i scope of
    Just val -> return val
    Nothing  -> findIdent i env { scope = rest }

defToClos :: Def -> Env -> Err Env
defToClos (DDef fun vars exp) env = 
  addIdent fun val env
  where val = defToClos' vars exp

-- TODO: Rename this
defToClos' :: [Ident] -> Exp -> Val
defToClos' []         exp        = Clos [] exp
defToClos' (var:vars) exp        = 
  Clos [var] $ defToClos' vars exp

findAndReplace :: Ident -> Exp -> Exp -> Exp
findAndReplace i from to =
  case from of
    EVar ident -> 
      if ident == i then VExp exp
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
    ELt e1 e2 exp -> ELt e1' e2' exp'
      where e1' = findAndReplace i e1 to
            e2' = findAndReplace i e2 to
            exp' = findAndReplace i exp to
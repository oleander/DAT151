module Interpreter where

import AbsCPP
import PrintCPP
import ErrM
import Control.Monad
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map

data Env = Env {
  scope :: [Map.Map Ident Exp],
  callByValue :: Bool
}

interpret :: Program -> Bool -> Err Exp
interpret (Prog defs) callBy = do
  env <- foldM defToClos (emptyEnv callBy) defs
  exp <- findIdent (Ident "main") env
  evalExp exp env

evalExp :: Exp -> Env -> Err Exp
evalExp exp env@Env{ callByValue = callByValue } =
  case exp of
    EVar i -> findIdent i env
    EInt n -> return $ EInt n
    EApp e1 e2 -> do
      val <- evalExp e1 env
      case val of
        (EAbs ident with) ->
          if callByValue then do
            exp' <- evalExp e2 env
            evalExp (findAndReplace ident with exp') (addBlock env)
          else 
            evalExp (findAndReplace ident with e2) (addBlock env)
        e                      -> fail $ "can't apply " ++ printTree e ++ " on " ++ printTree e
    EAbs i e -> return $ exp
    ESub e1 e2 -> binOp (-) e1 e2 env
    EAdd e1 e2 -> binOp (+) e1 e2 env
    EIf this a b -> do
      val <- evalExp this env
      case val of
        EInt 1 -> evalExp a env
        EInt 0 -> evalExp b env
        _      -> fail $ printTree val ++ " is not an int"
    ELt e1 e2 -> do
      v1 <- evalExp e1 env
      v2 <- evalExp e2 env
      case (v1, v2) of
        (EInt a, EInt b) -> 
          if a < b then return $ EInt 1
          else          return $ EInt 0
        (a           ,b) -> fail $ "can't compare\n" ++ printTree a ++ "\nwith\n" ++  printTree b

binOp :: (Integer -> Integer -> Integer) -> Exp -> Exp -> Env -> Err Exp
binOp op e1 e2 env = do
  v1 <- evalExp e1 env
  v2 <- evalExp e2 env
  case (v1, v2) of
    (EInt a, EInt b) -> return $ EInt (a `op` b)
    (a,           b) -> fail $ "can't apply op to " 
      ++ printTree a ++ " and " ++ printTree b

emptyEnv :: Bool -> Env
emptyEnv callBy = Env {
  scope      = [Map.empty],
  callByValue = callBy
}

addBlock :: Env -> Env
addBlock env@Env { scope = scope } = 
  env { scope = (Map.empty : scope) }

-- Add ident to scope
addIdent :: Ident -> Exp -> Env -> Err Env
addIdent i val env@Env{ scope = (scope:rest) } = 
  if Map.member i scope
  then fail $ show i ++ " already declared"
  else return $ env { scope = (Map.insert i val scope) : rest }

-- Find ident in scope
findIdent :: Ident -> Env -> Err Exp
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

defToClos' :: [Ident] -> Exp -> Exp
defToClos' []         exp        = exp
defToClos' (var:vars) exp        = 
  EAbs var $ (defToClos' vars exp)

-- Find an unbound {i} in {from} and replace with {to}
-- One might also add {i} to the environment and 
-- continue the evalution
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
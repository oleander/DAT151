{-# LANGUAGE RankNTypes #-}
module Interpreter where

import AbsCPP
import PrintCPP
import ErrM

import Control.Monad
import qualified Data.Map as M
import Data.IORef
import Data.Maybe

data Val = VInt Int | Clos Ident Exp deriving(Show)

main a b = z

--newtype Ident = Ident String deriving (Eq,Ord,Show,Read)
--data Program =
--   Prog [Def]
--  deriving (Eq,Ord,Show,Read)

--data Def =
--   DDef Ident [Ident] Exp
--  deriving (Eq,Ord,Show,Read)

--data Exp =
--   EVar Ident
-- | EApp Exp Exp
-- | EAbs Ident Exp
--  deriving (Eq,Ord,Show,Read)


interpret :: Program -> Bool -> Err Val
interpret (Prog defs) _ = undefined

evalExp :: Exp -> Env -> Err Val
evalExp exp env =
  case exp of
    EInt n -> return $ VInt n
    EApp e1 e2 -> undefined
    EAbs i exp -> undefined
    ESub e1 e2 -> binOp (-) e1 e2 env
    EAdd e1 e2 -> binOp (-) e1 e2 env
    EIf this a b ->
      case evalExp this env of
        VInt 1 -> evalExp a env
        VInt 0 -> evalExp b env
        exp    -> fail $ show exp ++ " is not an int"
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




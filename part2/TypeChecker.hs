module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
-- TODO: Check all defs, not just the first one
typecheck :: Program -> Err ()
typecheck (PDefs defs) = typecheckDef $ defs !! 0

typecheckDef :: Def -> Err ()
typecheckDef (DFun rType (Id name) args statements) =
  case inferStm statements of
    Ok t -> 
      if t == rType
      then Ok ()
      else Bad $ "wrong return type in " ++ name
    Bad str -> Bad str

-- Fetches the first return type from statement list
inferStm :: [Stm] -> Err Type
inferStm []                   = Bad "no return found"
inferStm ((SReturn exp):stms) = inferExp exp
inferStm (_:stms)             = inferStm stms

-- n--
inferExp :: Exp -> Err Type
inferExp (EPDecr exp) =
  case inferExp exp of
    (Ok Type_int)    -> Ok Type_int
    (Ok Type_double) -> Ok Type_double
    _      -> Bad "wrong type"

-- n++
inferExp (EPIncr exp) =
  case inferExp exp of
    (Ok Type_int)    -> Ok Type_int
    (Ok Type_double) -> Ok Type_double
    _      -> Bad "wrong type"

-- ++n
inferExp (EIncr exp) =
  case inferExp exp of
    (Ok Type_int)    -> Ok Type_int
    (Ok Type_double) -> Ok Type_double
    _      -> Bad "wrong type"

-- --n
inferExp (EDecr exp) =
  case inferExp exp of
    (Ok Type_int)    -> Ok Type_int
    (Ok Type_double) -> Ok Type_double
    _      -> Bad "wrong type"

inferExp (ETimes a b) =
  case inferExp a of
    (Ok Type_double) -> inferExp b
    (Ok Type_int)    -> inferExp b
    (Ok t)           -> Bad $ "can't run times on " ++ (show t)
    e                -> e

-- Regular int
inferExp (EInt i) = Ok Type_int

-- Bools
inferExp ETrue  = Ok Type_bool
inferExp EFalse = Ok Type_bool

inferExp (EDouble d) = Ok Type_double
inferExp (EId id)    = Bad "Can't check type of id"


inferExp e = error $ show e ++ " is not implemented yet"
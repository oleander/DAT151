module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Debug.Trace

-- TODO: Check all defs, not just the first one
typecheck :: Program -> Err ()
typecheck (PDefs defs) = trace (show defs) (typecheckDef $ defs !! 0)

typecheckDef :: Def -> Err ()
typecheckDef (DFun rType (Id name) args statements) =
  case findReturnType $ inferStm statements of
    Ok t -> 
      -- Do not check return type of "main"
      if ((name == "main") || t == rType)
      then Ok ()
      else Bad $ "wrong return type in " ++ name
    Bad str -> Bad str

findReturnType :: [Err Type] -> Err Type
findReturnType [] = Ok Type_void
findReturnType ((Ok r):xs) = Ok r
findReturnType (_:xs) = findReturnType xs

-- Fetches the first return type from statement list
inferStm :: [Stm] -> [Err Type]
inferStm []                   = [Ok Type_void]
inferStm (s:stms)             = 
  case s of
    SReturn e     -> (inferExp e) : inferStm stms
    SExp    e     -> (inferExp e) : inferStm stms
    SWhile  e stm -> 
      case inferExp e of
        Ok Type_bool -> inferStm [stm]
        Ok t         -> [Bad $ "only bool allowed in while not " ++ (show t)]
        st           -> [st]
    SBlock stms      -> inferStm stms
    SIfElse e s1 s2  ->
      case inferExp e of
        Ok Type_bool -> (inferStm [s1]) ++ (inferStm [s2])
        Ok t         -> [Bad $ "only bool allowed in if else not " ++ (show t)]
        st           -> [st]
    s'         -> (Bad $ "statement " ++ (show s') ++ " not matched") : inferStm stms

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

-- a * b
inferExp (ETimes a b) =
  case inferExp a of
    (Ok Type_double) -> Ok Type_double
    (Ok Type_int)    -> inferExp b
    (Ok t)           -> Bad $ "can't run times on " ++ (show t)
    e                -> e

-- a / b
inferExp (EDiv a b) =
  case inferExp a of
    (Ok Type_double) -> Ok Type_double
    (Ok Type_int)    -> inferExp b
    (Ok t)           -> Bad $ "can't run div on " ++ (show t)
    e                -> e

-- a + b
inferExp (EPlus a b) =
  case inferExp a of
    (Ok Type_double) -> 
      case inferExp b of
        -- (Ok Type_string) -> Bad "can't run + on double and string"
        (Ok Type_double) -> Ok Type_double
        (Ok Type_int)    -> Ok Type_double
        (Ok t)           -> Bad $ "can't run + on double and " ++ (show t)
    (Ok Type_int)    -> 
      case inferExp b of
        -- (Ok Type_string) -> Bad "can't run + on int and string"
        (Ok Type_double) -> Ok Type_double
        (Ok Type_int)    -> Ok Type_int
        (Ok t)           -> Bad $ "can't run + on int and " ++ (show t)
    --(Ok Type_string) -> 
    --  case inferExp b of
    --    (Ok Type_string) -> Ok Type_string
    --    (Ok t)           -> Bad $ "can't run + on string and " ++ (show t)
    (Ok t)           -> Bad $ "can't run plus on " ++ (show t)

-- a - b
inferExp (EMinus a b) =
  case inferExp a of
    (Ok Type_double) -> Ok Type_double
    (Ok Type_int)    -> inferExp b
    (Ok t)           -> Bad $ "can't run minus on " ++ (show t)
    e                -> e

-- a < b
inferExp (ELt a b) =
  case inferExp a of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run < on " ++ (show t)
    e                -> e

-- a > b
inferExp (EGt a b) =
  case inferExp a of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run < on " ++ (show t)
    e                -> e

-- a <= b
inferExp (ELtEq a b) =
  case inferExp a of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run < on " ++ (show t)
    e                -> e

-- a >= b
inferExp (EGtWq a b) =
  case inferExp a of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run < on " ++ (show t)
    e                -> e
    -- char *n = "hello"

-- a == b
inferExp (EEq a b) =
  case inferExp a of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    -- (Ok Type_string) -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run == on " ++ (show t)
    e                -> e

-- a != b
inferExp (ENEq a b) =
  case inferExp a of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    -- (Ok Type_string) -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run == on " ++ (show t)
    e                -> e

-- a && b
inferExp (EAnd a b) =
  case ((inferExp a), (inferExp b)) of
    ((Ok Type_bool), (Ok Type_bool)) -> Ok Type_bool
    e                                 -> Bad "can't run && on non bools"

-- a || b
inferExp (EOr a b) =
  case ((inferExp a), (inferExp b)) of
    ((Ok Type_bool), (Ok Type_bool)) -> Ok Type_bool
    e                                -> Bad "can't run || on non bools"

-- Regular int
inferExp (EInt i) = Ok Type_int

-- Bools
inferExp ETrue  = Ok Type_bool
inferExp EFalse = Ok Type_bool

inferExp (EDouble d) = Ok Type_double
inferExp (EId id)    = Bad "Can't check type of id"


inferExp e = error $ show e ++ " is not implemented yet"
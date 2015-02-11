module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Debug.Trace
import Environment

-- TODO: Check all defs, not just the first one
typecheck :: Program -> Err ()
typecheck (PDefs defs) = trace (show defs) (typecheckDef (defs !! 0) emptyFrame)

typecheckDef :: Def -> Environment -> Err ()
typecheckDef (DFun rType (Id name) args statements) g =
  case findReturnType $ inferStm statements g of
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
inferStm :: [Stm] -> Environment -> [Err Type]
inferStm []                   _ = [Ok Type_void]
inferStm (s:stms)             g = 
  case s of
    SReturn e     -> (inferExp e g) : inferStm stms g
    SExp    e     -> (inferExp e g) : inferStm stms g
    SWhile  e stm -> 
      case inferExp e g of
        Ok Type_bool -> inferStm [stm] g
        Ok t         -> [Bad $ "only bool allowed in while not " ++ (show t)]
        st           -> [st]
    -- { # block of code }
    SBlock stms      -> inferStm stms (newFrame g)
    -- int a = 10;
    SInit t i e      -> 
      case add i t g of
        Ok g' -> 
          case inferExp e g of -- Check type of e
            Ok t' ->
              if t == t'
              then inferStm stms g' -- Type is okay
              else [Bad $ "could not assign type " ++ (show t) ++ " to " ++ (show t')]
            s     -> [s]
        Bad env    -> [Bad $ "could not update env with type " ++ (show t)]
    -- if e { s1 } else { s2 }
    SIfElse e s1 s2  ->
      case inferExp e g of
        Ok Type_bool -> (inferStm [s1] g) ++ (inferStm [s2] g)
        Ok t         -> [Bad $ "only bool allowed in if else not " ++ (show t)]
        st           -> [st]
    SDecls t ids     -> undefined

-- n--
inferExp :: Exp -> Environment -> Err Type
inferExp (EPDecr exp) g =
  case inferExp exp g of
    (Ok Type_int)    -> Ok Type_int
    (Ok Type_double) -> Ok Type_double
    _      -> Bad "wrong type"

-- n++
inferExp (EPIncr exp) g =
  case inferExp exp g of
    (Ok Type_int)    -> Ok Type_int
    (Ok Type_double) -> Ok Type_double
    _      -> Bad "wrong type"

-- ++n
inferExp (EIncr exp) g =
  case inferExp exp g of
    (Ok Type_int)    -> Ok Type_int
    (Ok Type_double) -> Ok Type_double
    _      -> Bad "wrong type"

-- --n
inferExp (EDecr exp) g =
  case inferExp exp g of
    (Ok Type_int)    -> Ok Type_int
    (Ok Type_double) -> Ok Type_double
    _      -> Bad "wrong type"

-- a * b
inferExp (ETimes a b) g =
  case inferExp a g of
    (Ok Type_double) -> Ok Type_double
    (Ok Type_int)    -> inferExp b g
    (Ok t)           -> Bad $ "can't run times on " ++ (show t)
    e                -> e

-- a / b
inferExp (EDiv a b) g =
  case inferExp a g of
    (Ok Type_double) -> Ok Type_double
    (Ok Type_int)    -> inferExp b g
    (Ok t)           -> Bad $ "can't run div on " ++ (show t)
    e                -> e

-- a + b
inferExp (EPlus a b) g =
  case inferExp a g of
    (Ok Type_double) -> 
      case inferExp b g of
        -- (Ok Type_string) -> Bad "can't run + on double and string"
        (Ok Type_double) -> Ok Type_double
        (Ok Type_int)    -> Ok Type_double
        (Ok t)           -> Bad $ "can't run + on double and " ++ (show t)
    (Ok Type_int)    -> 
      case inferExp b g of
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
inferExp (EMinus a b) g =
  case inferExp a g of
    (Ok Type_double) -> Ok Type_double
    (Ok Type_int)    -> inferExp b g
    (Ok t)           -> Bad $ "can't run minus on " ++ (show t)
    e                -> e

-- a < b
inferExp (ELt a b) g =
  case inferExp a g of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run < on " ++ (show t)
    e                -> e

-- a > b
inferExp (EGt a b) g =
  case inferExp a g of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run < on " ++ (show t)
    e                -> e

-- a <= b
inferExp (ELtEq a b) g =
  case inferExp a g of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run < on " ++ (show t)
    e                -> e

-- a >= b
inferExp (EGtWq a b) g =
  case inferExp a g of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run < on " ++ (show t)
    e                -> e
    -- char *n = "hello"

-- a == b
inferExp (EEq a b) g =
  case inferExp a g of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    -- (Ok Type_string) -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run == on " ++ (show t)
    e                -> e

-- a != b
inferExp (ENEq a b) g =
  case inferExp a g of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    -- (Ok Type_string) -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run == on " ++ (show t)
    e                -> e

-- a && b
inferExp (EAnd a b) g =
  case ((inferExp a g), (inferExp b g)) of
    ((Ok Type_bool), (Ok Type_bool)) -> Ok Type_bool
    e                                 -> Bad "can't run && on non bools"

-- a || b
inferExp (EOr a b) g =
  case ((inferExp a g), (inferExp b g)) of
    ((Ok Type_bool), (Ok Type_bool)) -> Ok Type_bool
    e                                -> Bad "can't run || on non bools"

-- Regular int
inferExp (EInt i) _ = Ok Type_int

-- Bools
inferExp ETrue  _ = Ok Type_bool
inferExp EFalse _ = Ok Type_bool

inferExp (EDouble d) _ = Ok Type_double
inferExp (EId i)    g = find i g

inferExp (EAss e1 e2) g 
  | t1 == t2  = t1
  | otherwise = Bad $ "could not match type " ++  show t1 ++ " with " ++ show t2
  where
    t1 = inferExp e1 g
    t2 = inferExp e2 g

inferExp e g = error $ show e ++ " is not implemented yet"
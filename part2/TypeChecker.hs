module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Environment

typecheck :: Program -> Environment -> Err ()
typecheck (PDefs defs) g = 
  case combineEnv defs g of
    Ok g' -> 
      case combineDefs defs emptyFunScope of
        Ok scope -> typecheck' defs g' scope
        Bad e      -> Bad e
    Bad err -> Bad err

combineDefs :: [Def] -> GlobalFunScope -> Err GlobalFunScope
combineDefs [] g                          = Ok g
combineDefs ((DFun _ name args _):defs) g = 
  case addA name args g of
    Ok g' -> combineDefs defs g'
    bad -> bad
  
-- TODO: Rename this. Helper function for typecheck
typecheck' :: [Def] -> Environment -> GlobalFunScope -> Err ()
typecheck' []          _ _ = Ok ()
typecheck' (def:defs) g scope = 
  -- Create a new scope (frame) for function
  case typecheckDef def (newFrame g) scope of
    Ok () -> typecheck' defs g scope
    Bad err -> Bad err

-- Add args to env
combineArgs :: [Arg] -> Environment -> Err Environment
combineArgs []                 g = Ok g
combineArgs ((ADecl t i):args) g = 
  case add i t g of
    Ok g' -> combineArgs args g'
    bad -> bad

-- Add function return type to env
combineEnv :: [Def] -> Environment -> Err Environment
combineEnv [] g = Ok g
combineEnv ((DFun rType name args statements):rest) g =
  case add name rType g of
    Ok g' -> combineEnv rest g'
    Bad err -> Bad err

-- Fetches args list from scope using id, then compares with exps
compareWithArgs :: Environment -> GlobalFunScope -> Id -> [Exp] -> Err ()
compareWithArgs g scope i exps = 
  case findA i scope of
    Ok args -> ofSameType args exps g scope
    Bad err     -> Bad err

-- Is the args list of the same type as exps?
ofSameType :: [Arg] -> [Exp] -> Environment -> GlobalFunScope -> Err ()
ofSameType [] [] _ _ = Ok ()
ofSameType [] _ _ _ = Bad "wrong number of arguments"
ofSameType _ [] _ _ = Bad "wrong number of arguments"
ofSameType ((ADecl t i):args) (exp:exps) g scope = 
  case inferExp exp g scope of
    Ok t' -> 
      if t' == t
      then ofSameType args exps g scope
      else Bad "wrong type of arguments"
    Bad bad -> Bad bad

typecheckDef :: Def -> Environment -> GlobalFunScope -> Err ()
typecheckDef (DFun rType (Id name) args statements) g scope =
  case combineArgs args g of
    Ok g' -> 
      case findReturnType $ inferStm statements g' scope of
        Ok t -> 
          -- Do not check return type of "main"
          if ((name == "main") || t == rType)
          then Ok ()
          else Bad $ "wrong return type in " ++ name ++ " can't match " ++ (show t) ++ " with " ++ (show rType)
        Bad s -> Bad s
    Bad err -> Bad err

findReturnType :: [Err Type] -> Err Type
findReturnType [] = Ok Type_void
findReturnType ((Ok r):xs) = Ok r
findReturnType ((Bad x):xs) = Bad x

inferStm :: [Stm] -> Environment -> GlobalFunScope -> [Err Type]
inferStm []           _ _ = [Ok Type_void]
inferStm (s:stms) g scope = 
  case s of
    -- return e;
    SReturn e     -> (inferExp e g scope) : inferStm stms g scope
    -- exp ;
    SExp    e     -> 
      case inferExp e g scope of
        Ok _ -> inferStm stms g scope
        Bad b -> [Bad b]
    -- while(e) { stm }
    SWhile  e stm -> 
      case inferExp e g scope of
        Ok Type_bool -> inferStm [stm] g scope
        Ok t         -> [Bad $ "only bool allowed in while not " ++ (show t)]
        st           -> [st]
    -- { # block of code }
    SBlock stms      -> inferStm stms (newFrame g) scope
    -- int a = 10;
    SInit t i e      -> 
      case add i t g of
        Ok g' -> 
          case inferExp e g scope of -- Check type of e
            Ok t' ->
              if t == t'
              then inferStm stms g' scope -- Type is okay
              else [Bad $ "could not assign type " ++ (show t) ++ " to " ++ (show t')]
            s     -> [s]
        Bad env    -> [Bad $ "could not update env with type " ++ (show t)]
    -- if e { s1 } else { s2 }
    SIfElse e s1 s2 ->
      case inferExp e g scope of
        Ok Type_bool -> (inferStm [s1] g scope) ++ (inferStm [s2] g scope)
        Ok t         -> [Bad $ "only bool allowed in if else not " ++ (show t)]
        st           -> [st]
    -- int a,b,c;
    SDecls t ids     ->
      case combineDec t ids g of
        Ok g' -> inferStm stms g' scope
        Bad err -> [Bad err]

-- Add a list of args to to gamma
combineDec :: Type -> [Id] -> Environment -> Err Environment
combineDec _ [] g = Ok g
combineDec t (i:ids) g = 
  case add i t g of
    Ok g' -> combineDec t ids g'
    bad -> bad

-- n--
inferExp :: Exp -> Environment -> GlobalFunScope -> Err Type
inferExp (EPDecr exp) g scope =
  case inferExp exp g scope of
    (Ok Type_int)    -> Ok Type_int
    (Ok Type_double) -> Ok Type_double
    _      -> Bad "wrong type"

-- n++
inferExp (EPIncr exp) g scope =
  case inferExp exp g scope of
    (Ok Type_int)    -> Ok Type_int
    (Ok Type_double) -> Ok Type_double
    _      -> Bad "wrong type"

-- ++n
inferExp (EIncr exp) g scope =
  case inferExp exp g scope of
    (Ok Type_int)    -> Ok Type_int
    (Ok Type_double) -> Ok Type_double
    _      -> Bad "wrong type"

-- --n
inferExp (EDecr exp) g scope =
  case inferExp exp g scope of
    (Ok Type_int)    -> Ok Type_int
    (Ok Type_double) -> Ok Type_double
    _      -> Bad "wrong type"

-- a * b
inferExp (ETimes a b) g scope =
  case inferExp a g scope of
    (Ok Type_double) -> Ok Type_double
    (Ok Type_int)    -> inferExp b g scope
    (Ok t)           -> Bad $ "can't run times on " ++ (show t)
    e                -> e

-- a / b
inferExp (EDiv a b) g scope =
  case inferExp a g scope of
    (Ok Type_double) -> Ok Type_double
    (Ok Type_int)    -> inferExp b g scope
    (Ok t)           -> Bad $ "can't run div on " ++ (show t)
    e                -> e

-- a + b
inferExp (EPlus a b) g scope =
  case inferExp a g scope of
    (Ok Type_double) -> 
      case inferExp b g scope of
        -- (Ok Type_string) -> Bad "can't run + on double and string"
        (Ok Type_double) -> Ok Type_double
        (Ok Type_int)    -> Ok Type_double
        (Ok t)           -> Bad $ "can't run + on double and " ++ (show t)
    (Ok Type_int)    -> 
      case inferExp b g scope of
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
inferExp (EMinus a b) g scope =
  case inferExp a g scope of
    (Ok Type_double) -> Ok Type_double
    (Ok Type_int)    -> inferExp b g scope
    (Ok t)           -> Bad $ "can't run minus on " ++ (show t)
    e                -> e

-- a < b
inferExp (ELt a b) g scope =
  case inferExp a g scope of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run < on " ++ (show t)
    e                -> e

-- a > b
inferExp (EGt a b) g scope =
  case inferExp a g scope of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run < on " ++ (show t)
    e                -> e

-- a <= b
inferExp (ELtEq a b) g scope =
  case inferExp a g scope of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run < on " ++ (show t)
    e                -> e

-- a >= b
inferExp (EGtWq a b) g scope =
  case inferExp a g scope of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run < on " ++ (show t)
    e                -> e
    -- char *n = "hello"

-- a == b
inferExp (EEq a b) g scope =
  case inferExp a g scope of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    -- (Ok Type_string) -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run == on " ++ (show t)
    e                -> e

-- a != b
inferExp (ENEq a b) g scope =
  case inferExp a g scope of
    (Ok Type_double) -> Ok Type_bool
    (Ok Type_int)    -> Ok Type_bool
    -- (Ok Type_string) -> Ok Type_bool
    (Ok t)           -> Bad $ "can't run == on " ++ (show t)
    e                -> e

-- a && b
inferExp (EAnd a b) g scope =
  case ((inferExp a g scope), (inferExp b g scope)) of
    ((Ok Type_bool), (Ok Type_bool)) -> Ok Type_bool
    e                                -> Bad "can't run && on non bools"

-- a || b
inferExp (EOr a b) g scope =
  case ((inferExp a g scope), (inferExp b g scope)) of
    ((Ok Type_bool), (Ok Type_bool)) -> Ok Type_bool
    e                                -> Bad "can't run || on non bools"

-- Regular int
inferExp (EInt i) _ _ = Ok Type_int

-- Bools
inferExp ETrue  _ _ = Ok Type_bool
inferExp EFalse _ _ = Ok Type_bool

inferExp (EDouble d) _ _ = Ok Type_double
inferExp (EId i)    g _ = find i g

-- a = b
inferExp (EAss e1 e2) g scope 
  | t1 == t2  = t1
  | otherwise = Bad $ "could not match type " ++  show t1 ++ " with " ++ show t2
  where
    t1 = inferExp e1 g scope
    t2 = inferExp e2 g scope

-- myFun(a,b,c)
inferExp (EApp i exps) g scope = 
  case compareWithArgs g scope i exps of
    Ok () -> find i g
    Bad bad -> Bad bad
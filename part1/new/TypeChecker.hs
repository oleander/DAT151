module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Environment
import Control.Monad

data TypeData = FunT {returnType :: Type, argTypes :: [Type]}
              | VarT Type
type Env = Environment Id TypeData

typecheck :: Program -> Err ()
typecheck (PDefs defs) = do
  let defToEnvPair (DFun t i args _) =
        (i, FunT t (map (\(ADecl argT _) -> argT) args))
      builtIns = map (\(i, r, a) -> (Id i, FunT r a))
                 [("printInt", Type_void, [Type_int])
                 , ("printDouble", Type_void, [Type_double])
                 , ("readInt", Type_int, [])
                 , ("readDouble", Type_double, [])]
  startEnv <- newEnv $ (builtIns ++ map defToEnvPair defs)
  mapM_ (checkDef startEnv) defs

data WType = WType Type | None deriving (Show, Eq)

checkStm :: Env -> Stm -> Err Env
checkStm env s = case s of
  SExp e -> do
    typeOfExp e env >> return env
  SDecls t ids ->
    foldM (\env' i -> addDecl env' i t) env ids
  SInit t1 i e -> do
    t2 <- typeOfExp e env
    checkEqual (wrapT t1) (wrapT t2) >> addDecl env i t1
  SReturn e -> do
    returnT <- findVarT (Id "__RETURN__") env
    t1 <- typeOfExp e env
    checkEqual (wrapT t1) (wrapT returnT) >> return env
  SWhile e stm -> do
    t1 <- typeOfExp e env
    checkEqual (wrapT t1) (wrapT Type_bool)
    _ <- checkStm env stm
    return env
  SBlock stms -> do
    foldM_ checkStm (newFrame env) stms
    return env
  SIfElse e stma stmb -> do
    t1 <- typeOfExp e env
    checkEqual (wrapT t1) (wrapT Type_bool)
    _ <- checkStm env stma
    _ <- checkStm env stmb
    return env

-- actual -> expected -> message
checkEqual :: WType -> WType -> Err ()
checkEqual actual None = return ()
checkEqual actual expected =
  if actual == expected
  then return ()
  else fail ("Types do not match " ++ show actual
             ++ " (actual) " ++ show expected ++ " (expected)")

wrapT :: Type -> WType
wrapT t = WType t

addDecl :: Env -> Id -> Type -> Err Env
addDecl env i t = add i (VarT t) env

checkDef :: Env -> Def -> Err ()
checkDef env (DFun t _ args stms) = do
  env' <- addFrame env $
          -- Hack: store the return type (needed to typecheck return
          -- statements in the environment.
          ([(Id "__RETURN__", VarT t)])
          ++ map (\(ADecl argT i) -> (i, VarT argT)) args
  _ <- checkStm env' $ SBlock stms
  return ()

-- FIXME DRY
findFun :: Id -> Env -> Err TypeData
findFun i env = do
  res <- find i env
  case res of
    FunT _ _ -> return res
    _ -> fail $ "Not a function!" ++ show i
findFunReturnT :: Id -> Env -> Err Type
findFunReturnT i env = fmap returnType $ findFun i env
findFunArgTs i env = fmap argTypes $ findFun i env
findVarT i env = do
  res <- find i env
  case res of
    VarT t -> return t
    _ -> fail $ "Not a variable!" ++ show i

selectHighest :: Type -> Type -> Type 
selectHighest Type_double Type_int = Type_double
selectHighest Type_int Type_double = Type_double
selectHighest Type_int _ = Type_int
selectHighest Type_double _ = Type_double
selectHighest _ _ = error "not yet defined"

typeCheckDuo :: Exp -> Exp -> Env -> Err Type
typeCheckDuo a b env = do
  t1 <- typeOfExp a env
  t2 <- typeOfExp b env
  (checkVagueEqual t1 t2) >> (return $ selectHighest t1 t2)

typeCheckComp :: Exp -> Exp -> Env -> Err Type
typeCheckComp a b env = do
  t1 <- typeOfExp a env
  t2 <- typeOfExp b env
  (checkVagueEqual t1 t2) >> (return Type_bool)

typeCheckBool :: Exp -> Exp -> Env -> Err Type
typeCheckBool a b env = do
  t1 <- typeOfExp a env
  t2 <- typeOfExp b env
  case (t1, t2) of
    (Type_bool, Type_bool) -> return Type_bool
    _                      -> fail $ "can't match " ++ show t1 ++ " with " ++ show t2

checkVagueEqual :: Type -> Type -> Err ()
checkVagueEqual Type_double Type_int = return ()
checkVagueEqual Type_int Type_double = return ()
checkVagueEqual Type_int _ = return ()
checkVagueEqual Type_double _ = return ()
checkVagueEqual Type_bool Type_bool = return ()
checkVagueEqual Type_void Type_void = return ()
checkVagueEqual a b = fail $ "type " ++ (show a) ++ " don't match " ++ (show b)

checkArgsWithParams :: [Type] -> [Exp] -> Env -> Err ()
checkArgsWithParams [] [] _ = return ()
checkArgsWithParams [] _ _ = fail "wrong number of args"
checkArgsWithParams _ [] _ = fail "wrong number of args"
checkArgsWithParams (t1:types) (exp:exps) env = do
  t2 <- typeOfExp exp env
  (checkEqual (wrapT t1) (wrapT t2)) >> (checkArgsWithParams types exps env)

typeCheckEq :: Exp -> Exp -> Env -> Err Type
typeCheckEq a b env = do
  t1 <- typeOfExp a env
  t2 <- typeOfExp b env
  checkEqual (wrapT t1) (wrapT t2) >> return Type_bool

typeOfExp :: Exp -> Env -> Err Type
typeOfExp ETrue _ = return Type_bool
typeOfExp EFalse _ = return Type_bool
typeOfExp (EInt _) _ = return Type_int
typeOfExp (EDouble _) env = return Type_double
typeOfExp (EId i) env = findVarT i env
typeOfExp (EApp i exps) env = do
  returnT <- findFunReturnT i env
  argTypes <- findFunArgTs i env
  (checkArgsWithParams argTypes exps env) >> return returnT
typeOfExp (EPDecr e) env = typeOfExp e env
typeOfExp (EPIncr e) env = typeOfExp e env
typeOfExp (EIncr e) env = typeOfExp e env
typeOfExp (EDecr e) env = typeOfExp e env
typeOfExp (ETimes a b) env = typeCheckDuo a b env
typeOfExp (EDiv a b) env = typeCheckDuo a b env
typeOfExp (EPlus a b) env = typeCheckDuo a b env
typeOfExp (EMinus a b) env = typeCheckDuo a b env
typeOfExp (ELt a b) env = typeCheckComp a b env
typeOfExp (EGt a b) env = typeCheckComp a b env
typeOfExp (ELtEq a b) env = typeCheckComp a b env
typeOfExp (EGtWq a b) env = typeCheckComp a b env
typeOfExp (EEq a b) env = typeCheckEq a b env
typeOfExp (ENEq a b) env = typeCheckEq a b env
typeOfExp (EAnd a b) env = typeCheckBool a b env
typeOfExp (EOr a b) env = typeCheckBool a b env
typeOfExp (EAss a b) env = do 
  t1 <- typeOfExp a env
  t2 <- typeOfExp b env
  (checkEqual (wrapT t1) (wrapT t2)) >> return t2
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
    checkExp e None env
    return env
  SDecls t ids ->
    foldM (\env' i -> addDecl env' i t) env ids
  SInit t i e -> do
    checkExp e (WType t) env
    addDecl env i t
  SReturn e -> do
    returnT <- findVarT (Id "__RETURN__") env
    checkExp e (WType returnT) env
    return env
  SWhile e stm -> do
    checkExp e (WType Type_bool) env
    _ <- checkStm env stm
    return env
  SBlock stms -> do
    foldM_ checkStm (newFrame env) stms
    return env
  SIfElse e stma stmb -> do
    checkExp e (WType Type_bool) env
    _ <- checkStm env stma
    _ <- checkStm env stmb
    return env

checkExp :: Exp -> WType -> Env -> Err ()
checkExp ETrue t env  = checkEqual (WType Type_bool) t
checkExp EFalse t env = checkEqual (WType Type_bool) t
checkExp (EInt _) t env = checkEqual (WType Type_int) t
checkExp (EDouble _) t env = checkEqual (WType Type_double) t
checkExp (EId i) t env = do
  t' <- findVarT i env
  checkEqual (WType t') t
checkExp (EApp i exps) t env = do
  returnT <- findFunReturnT i env
  checkEqual (WType returnT) t
  argTypes <- findFunArgTs i env
  if (length exps == length argTypes)
    then forM_ (zip argTypes exps) $ \(argT, argExp) ->
    checkExp argExp (WType argT) env
    else fail "Not enough arguments"
checkExp (EPDecr e) t env = checkPrePost e env t
checkExp (EPIncr e) t env = checkPrePost e env t
checkExp (EIncr e) t env = checkPrePost e env t
checkExp (EDecr e) t env = checkPrePost e env t
checkExp (ETimes a b) t env = checkNumBinOp a b t env
checkExp (EDiv a b) t env = checkNumBinOp a b t env
checkExp (EPlus a b) t env = checkNumBinOp a b t env
checkExp (EMinus a b) t env = checkNumBinOp a b t env
-- FIXME these aren't right
checkExp (ELt a b) t env = checkCompBinOp a b t env
checkExp (EGt a b) t env = checkCompBinOp a b t env
checkExp (ELtEq a b) t env = checkCompBinOp a b t env
checkExp (EGtWq a b) t env = checkCompBinOp a b t env
checkExp (EEq a b) t env = checkCompBinOp a b t env
checkExp (ENEq a b) t env = checkCompBinOp a b t env
checkExp (EAnd a b) t env = checkBoolBinOp a b t env
checkExp (EOr a b) t env = checkBoolBinOp a b t env
checkExp (EAss a b) t env = checkBinOp a b t env
-- checkEqual _ Type_void _ = return ()

checkPrePost :: Exp -> Env -> WType -> Err ()
checkPrePost (EId i) env rType = do
  idT <- findVarT i env
  if idT `elem` [Type_int, Type_double]
    then return ()
    else fail "Can only be applied to int or double"
  checkEqual (WType idT) rType
checkPrePost _ _ _ = do
  fail "Pre/post incr/decr can only be applied to variables"

checkBoolBinOp :: Exp -> Exp -> WType -> Env -> Err ()
checkBoolBinOp a b rType env = do
  checkEqual (WType Type_bool) rType
  checkBinOp a b (WType Type_bool) env

checkCompBinOp :: Exp -> Exp -> WType -> Env -> Err ()
checkCompBinOp a b t env = do
  checkEqual (WType Type_bool) t
  mplus ((checkExp a (WType Type_int) env) >> (checkExp b (WType Type_int) env))
        ((checkExp a (WType Type_double) env) >> (checkExp b (WType Type_double) env))

checkNumBinOp :: Exp -> Exp -> WType -> Env -> Err ()
checkNumBinOp a b t env = do
  mplus ((checkEqual (WType Type_int) t) >> checkBinOp a b (WType Type_int) env)
        (checkEqual (WType Type_double) t >> checkBinOp a b (WType Type_double) env)

checkBinOp :: Exp -> Exp -> WType -> Env -> Err ()
checkBinOp a b t env = do
  checkExp a t env
  checkExp b t env

-- actual -> expected -> message
checkEqual :: WType -> WType -> Err ()
checkEqual actual None = return ()
checkEqual actual expected =
  if actual == expected
  then return ()
  else fail ("Types do not match " ++ show actual
             ++ " (actual) " ++ show expected ++ " (expected)")

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

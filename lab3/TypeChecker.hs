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

checkStm :: Env -> Stm -> Err Env
checkStm env s = case s of
  SExp e -> do
    checkExp e Type_void env
    return env
  SDecls t ids ->
    foldM (\env' i -> addDecl env' i t) env ids
  SInit t i e -> do
    checkExp e t env
    addDecl env i t
  SReturn e -> do
    returnT <- findVarT (Id "__RETURN__") env
    checkExp e returnT env
    return env
  SWhile e stm -> do
    checkExp e Type_bool env
    _ <- checkStm env stm
    return env
  SBlock stms -> do
    foldM_ checkStm (newFrame env) stms
    return env
  SIfElse e stma stmb -> do
    checkExp e Type_bool env
    _ <- checkStm env stma
    _ <- checkStm env stmb
    return env

checkExp :: Exp -> Type -> Env -> Err ()
checkExp exp targetType env = check targetType env
  where check = case exp of
          ETrue -> checkEqual Type_bool
          EFalse -> checkEqual Type_bool
          EInt _ -> checkEqual Type_int
          EDouble _ -> checkEqual Type_double
          EId i -> \t _ -> do
            t' <- findVarT i env
            checkEqual t' t env
          EApp i exps -> \t _ -> do
            returnT <- findFunReturnT i env
            checkEqual returnT t env
            argTypes <- findFunArgTs i env
            if (length exps == length argTypes)
              then forM_ (zip argTypes exps) $ \(argT, argExp) ->
              checkExp argExp argT env
              else fail "Too little/not enough arguments"
          EPDecr e -> checkPrePost e
          EPIncr e -> checkPrePost e
          EIncr e -> checkPrePost e
          EDecr e -> checkPrePost e
          ETimes a b -> checkNumBinOp a b
          EDiv a b -> checkNumBinOp a b
          EPlus a b -> checkNumBinOp a b
          EMinus a b -> checkNumBinOp a b
          -- FIXME these aren't right
          ELt a b -> checkCompBinOp a b
          EGt a b -> checkCompBinOp a b
          ELtEq a b -> checkCompBinOp a b
          EGtWq a b -> checkCompBinOp a b
          EEq a b -> checkCompBinOp a b
          ENEq a b -> checkCompBinOp a b
          EAnd a b -> checkBoolBinOp a b
          EOr a b -> checkBoolBinOp a b
          EAss a b -> \t _ -> checkBinOp a b t
        checkEqual _ Type_void _ = return ()
        checkEqual t targetT _ =
          if t == targetT
          then return ()
          else fail ("Types do not match " ++ show t
                     ++ " (actual) " ++ show targetT ++ " (expected)")
        checkBoolBinOp a b t _ = do
          checkEqual Type_bool t env
          checkBinOp a b Type_bool
        checkCompBinOp a b t _ = do
          checkEqual Type_bool t env
          mplus ((checkExp a Type_int env) >> (checkExp b Type_int env))
                ((checkExp a Type_double env) >> (checkExp b Type_double env))
        checkNumBinOp a b t _ = do
          mplus (checkEqual Type_int t env >> checkBinOp a b Type_int)
                (checkEqual Type_double t env >> checkBinOp a b Type_double)
        checkBinOp a b t = do
          checkExp a t env
          checkExp b t env
        -- This is possible due to only identifiers being allowed for
        -- pre/postfix increments
        -- TODO use MonadPlus?
        checkPrePost (EId i) t _ = do
          idT <- findVarT i env
          if idT `elem` [Type_int, Type_double]
            then return ()
            else fail "Can only be applied to int or double"
          checkEqual idT t env
        checkPrePost _ _ _ = do
          fail "Pre/post incr/decr can only be applied to variables"

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

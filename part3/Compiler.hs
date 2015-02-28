module Compiler where

import AbsCPP
import PrintCPP
import ErrM

import Data.List.Split
import Data.List
import System.Cmd
import Control.Monad
import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.IORef
import Data.Maybe
import Control.Monad.IO.Class
import Data.IORef
import Debug.Trace

import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)

import Data.List (nub)

type Block = Map.Map String Integer
type Scope = [Block]
type Fun = (Type, [Arg])
type FunScope = Map.Map Id Fun
type Operator = String
type Counter = Int -> IO Int
data Env = Env { 
  scope :: Scope,
  functionScope :: FunScope
}

labelCntr :: IORef Integer
labelCntr = unsafePerformIO $ newIORef 0

addrCntr :: IORef Integer
addrCntr = unsafePerformIO $ newIORef 0

resetAddrCounter = writeIORef addrCntr 0

currentClass :: IORef String
currentClass = unsafePerformIO $ newIORef ""

outputFile :: IORef String
outputFile = unsafePerformIO $ newIORef ""

emptyScope :: Scope
emptyScope = [Map.empty]

emptyFunctionScope :: FunScope
emptyFunctionScope = Map.empty

newBlock :: Env -> Env
newBlock env@Env { scope = scope } = 
  env { scope = Map.empty : scope }

-- TODO: Free space by decrementing addrCounter
removeBlock :: Env -> Env
removeBlock env@Env { scope = scope } = 
  env { scope = tail scope }

lookupAddr :: Id -> Env -> Integer
lookupAddr (Id i) env@Env{ scope = scope' } =
  case scope' of
    []    -> error $ show i ++ " wasn't found"
    (f:e) ->
      case Map.lookup i f of
        Just val -> val
        Nothing -> lookupAddr (Id i) env { scope = e }

lookupFun :: Id -> Env -> Fun
lookupFun i env@Env { functionScope = scope } =
  case i of
    Id "readInt"      -> (Type_int, [])
    Id "printDouble"  -> error "not defined"
    Id "readDouble"   -> error "not defined"
    Id "printInt"     -> (Type_void, [ADecl Type_int (Id "x")])
    _                 -> case Map.lookup i scope of
                    Just val -> val
                    Nothing  -> error $ show i ++ " not found"
emptyEnv :: Env
emptyEnv = Env {
  scope         = emptyScope,
  functionScope = emptyFunctionScope
}

newLabel :: Env -> IO String
newLabel _ = do
  c <- readIORef labelCntr
  writeIORef labelCntr (c + 1)
  return $ "Label" ++ show c

extend :: Id -> Env -> IO Env
extend (Id i) env@Env{ scope = (block:s) } = do 
  counter <- readIORef addrCntr
  writeIORef addrCntr (counter + 1)
  if Map.member i block
  then error $ show i ++ " already declared"
  -- Always increment counter by one as we don't need doubles
  else return env { scope = (Map.insert i counter block) : s }

extendFun :: Def -> Env -> Env
extendFun (DFun t i args _) env@Env{ functionScope = functionScope } = 
  if Map.member i functionScope
  then error $ show i ++ " already declared"
  else env { functionScope = functionScope' }
  where
    functionScope' = Map.insert i (t, args) functionScope

compile :: Program -> String -> IO ()
compile (PDefs defs) filePath = do
  -- Store current class name as global var
  writeIORef currentClass klass'
  writeIORef outputFile newFile
  removeIfExists newFile
  writeFile newFile ""

  emit $ ".class public " ++ klass'
  emit ".super java/lang/Object"
  emit ".method public <init>()V"
  emit "aload_0"
  emit "invokespecial java/lang/Object/<init>()V"
  emit "return"
  emit ".end method"

  emit ".method public static main([Ljava/lang/String;)V"
  emit ".limit locals 1"
   
  emit $ "invokestatic " ++ klass' ++ "/main()I"
  emit "pop"
  emit "return"
  emit ".end method"

  mapM_ (\def -> compileDef def env) defs

  system $ "java -jar jasmin-2.4/jasmin.jar -d " ++ newFilePath ++ " " ++ newFile

  return ()

  where 
    env = foldr extendFun emptyEnv defs
    klass' = last $ splitOn "/" $ head $ splitOn "." filePath
    newFile = (head $ splitOn "." filePath) ++ ".c"
    newFilePath = "./" ++ (intercalate "/" $ init $ splitOn "/" filePath)

compileStms :: [Stm] -> Env -> Type -> IO ()
compileStms [] _               _ = return ()
compileStms (stm:stms) env rType =
  case stm of
    (SDecls t ids) -> do
      env' <- compressIdsWithEnv ids env
      compileStms stms env' rType
    (SInit t i exp) -> do
      env' <- extend i env
      compileExp exp env'
      emit $ "istore " ++ show (lookupAddr i env')
      compileStms stms env' rType
    (SExp exp) -> do
      compileExp exp env
      case exp of
        (EApp i _) ->
          case lookupFun i env of
            (Type_void, _) -> return ()
            (_, _)         -> emit "pop"
        _                  -> emit "pop"
      compileStms stms env rType
    (SBlock s) -> do
      compileStms s env' rType
      compileStms stms env'' rType
      where 
        env' = newBlock env
        env'' = removeBlock env'
    (SIfElse exp s1 s2) -> do
      done <- newLabel env
      false <- newLabel env
      compileExp exp env
      emit $ "ifeq " ++ false
      compileStms [s1] env rType
      emit $ "goto " ++ done
      emit $ false ++ ":"
      compileStms [s2] env rType
      emit $ done ++ ":"
      compileStms stms env rType
    (SWhile exp stm) -> do
      test <- newLabel env
      end <- newLabel env
      emit $ test ++ ":"
      compileExp exp env
      emit $ "ifeq " ++ end
      compileStms [stm] env rType
      emit $ "goto " ++ test
      emit $ end ++ ":"
      compileStms stms env rType
    (SReturn exp) -> do
      compileExp exp env
      case rType of
        Type_void -> emit "return"
        _         -> emit "ireturn"

compileDef :: Def -> Env -> IO ()
compileDef (DFun t (Id i) args stms) env = do
  resetAddrCounter
  env' <- compressArgsWithEnv args env
  emit $ ".method public static " ++ i ++ "(" ++ args' ++ ")" ++ (mapType t)
  emit ".limit locals 1000"
  emit ".limit stack 1000"
  compileStms stms env' t
  case (i, t) of
    ("main", Type_int) -> do
      emit "ldc 0"
      emit "ireturn"
    (_, Type_void) -> do
      emit "return"
    e -> return ()
  emit ".end method"
  where 
    args' = compressArgs args
compileExp :: Exp -> Env -> IO ()
compileExp (EInt i) env = 
  emit $ "ldc " ++ show i
compileExp (EPlus a b) env = do
  compileExp a env
  compileExp b env
  emit "iadd"
compileExp (EDiv a b) env = do
  compileExp a env
  compileExp b env
  emit "idiv"
compileExp (EId i) env = emit $ "iload " ++ show i'
  where i' = lookupAddr i env
compileExp (EApp (Id i) exps) env = do
  klass <- getClassFrom i
  mapM_ (\exp -> compileExp exp env) exps
  emit $ "invokestatic " ++ klass ++ "/" ++ i ++ "(" ++ args' ++ ")" ++ (mapType rType)
  where 
    args' = compressArgs args
    (rType, args) = lookupFun (Id i) env
-- n++
compileExp (EPIncr (EId i)) env = do
  compileExp (EId i) env
  emit "dup"
  emit "bipush 1"
  emit "iadd"
  emit $ "istore " ++ show i'
  where i' = lookupAddr i env
-- n--
compileExp (EPDecr (EId i)) env = do
  compileExp (EId i) env
  emit "dup"
  emit "bipush 1"
  emit "isub"
  emit $ "istore " ++ show i'
  where i' = lookupAddr i env
compileExp (EDouble exp) env = error "not defined for doubles"
compileExp (EOr e1 e2) env = do
  tryAgain <- newLabel env
  false <- newLabel env
  true <- newLabel env
  done <- newLabel env
  compileExp e1 env
  emit $ "ifeq " ++ tryAgain
  emit $ "goto " ++ true
  emit $ tryAgain ++ ":"
  compileExp e2 env
  emit $ "ifeq " ++ false
  emit $ true ++ ":"
  emit $ "bipush 1"
  emit $ "goto " ++ done
  emit $ false ++ ":"
  emit $ "bipush 0"
  emit $ done ++ ":"
compileExp (EAnd e1 e2) env = do
  false <- newLabel env
  done <- newLabel env
  compileExp e1 env
  emit $ "ifeq " ++ false
  compileExp e2 env
  emit $ "ifeq " ++ false
  emit $ "bipush 1"
  emit $ "goto " ++ done
  emit $ false ++ ":"
  emit $ "bipush 0"
  emit $ done ++ ":"
compileExp (EMinus e1 e2) env = do
  compileExp e1 env
  compileExp e2 env
  emit "isub"
compileExp (ETimes e1 e2) env = do
  compileExp e1 env
  compileExp e2 env
  emit "imul"
compileExp (EDecr (EId i)) env = do
  compileExp (EMinus (EId i) (EInt 1)) env
  emit $ "istore " ++ show i'
  emit $ "iload " ++ show i'
  where i' = lookupAddr i env
compileExp (EIncr (EId i)) env = do
  compileExp (EPlus (EId i) (EInt 1)) env
  emit $ "istore " ++ show i'
  emit $ "iload " ++ show i'
  where i' = lookupAddr i env
compileExp ETrue env = do
  emit "bipush 1"
compileExp EFalse env = do
  emit "bipush 0"
compileExp (ELtEq e1 e2) env = compareExp "if_icmple" e1 e2 env
compileExp (ELt e1 e2) env = compareExp "if_icmplt" e1 e2 env
compileExp (EGtWq e1 e2) env = compareExp "if_icmpge" e1 e2 env
compileExp (EGt e1 e2) env = compareExp "if_icmpgt" e1 e2 env
compileExp (ENEq e1 e2) env = compareExp "if_icmpne" e1 e2 env
compileExp (EEq e1 e2) env = compareExp "if_icmpeq" e1 e2 env
compileExp (EAss (EId i) exp) env = do
  compileExp exp env
  emit "dup"
  emit $ "istore " ++ show i'
  where i' = lookupAddr i env
compileExp exp _ = error $ "not implemented" ++ show exp

getClassFrom :: String -> IO String
getClassFrom klass =
  case klass of
  "readInt"      -> return "Runtime"
  "printDouble"  -> return "Runtime"
  "readDouble"   -> return "Runtime"
  "printInt"     -> return "Runtime"
  _              -> readIORef currentClass

-- int a, int b => II
-- bool a, int b => ZI
compressArgs :: [Arg] -> String
compressArgs []                 = ""
compressArgs ((ADecl t _):args) = (mapType t) ++ compressArgs args

compressArgsWithEnv :: [Arg] -> Env -> IO Env
compressArgsWithEnv [] env                  = return env
compressArgsWithEnv ((ADecl _ id):args) env = do
  env' <- extend id env
  compressArgsWithEnv args env'

compressIdsWithEnv :: [Id] -> Env -> IO Env
compressIdsWithEnv [] env       = return env
compressIdsWithEnv (id:ids) env = do
  env' <- extend id env
  compressIdsWithEnv ids env'

mapType :: Type -> String
mapType Type_bool = "Z"
mapType Type_int  = "I"
mapType Type_void = "V"
mapType t         = error $ "not defined for " ++ show t

compareExp :: Operator -> Exp -> Exp -> Env -> IO ()
compareExp operator e1 e2 env = do
  true <- newLabel env
  emit "bipush 1"
  compileExp e1 env
  compileExp e2 env
  emit $ operator ++ " " ++ true
  emit "pop"
  emit "bipush 0"
  emit $ true ++ ":"

emit :: String -> IO()
emit intruction = do 
  fileName <- readIORef outputFile
  appendFile fileName $ intruction ++ "\n"

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e
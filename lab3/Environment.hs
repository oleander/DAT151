module Environment where

import qualified Data.Map as Map
import ErrM
import AbsCPP
import Data.List (nub)

type Frame k v = Map.Map k v
type Environment k v = [Frame k v]

newEnv :: (Ord k, Show k) => [(k, v)] -> Err (Environment k v)
newEnv kvs = addFrame [] kvs

newFrame :: Environment k v -> Environment k v
newFrame e = Map.empty : e

-- TODO error when duplicate keys
addFrame :: (Ord k, Show k) =>
            Environment k v -> [(k, v)] -> Err (Environment k v)
addFrame env kvs = do
  if (map fst kvs) == (nub $ map fst kvs)
    then return $ Map.fromList kvs : env
    else fail "Already declared in this scope"

deleteFrame :: Environment k v -> Environment k v
deleteFrame = tail

emptyEnv :: Environment k v
emptyEnv = [Map.empty]

-- -- FIXME: This should fail with an Err
-- update :: (Ord k) => k -> v -> Environment k v -> Environment k v
-- update i v (f:e) =
--   if Map.member i f
--   then Map.insert i v f : e
--   else f : update i v e
-- update _ _ [] = error "could not update"
--update i v [] = Bad $ "could not update id " ++ (show i) ++
--  " with type " ++ (show v)

add :: (Ord k, Show k) => k -> v -> Environment k v -> Err (Environment k v)
add i v (f:e) =
  if Map.member i f
  then fail $ show i ++ " already declared"
  else return $ Map.insert i v f : e
add _ _ [] = fail "no environment found"

find :: (Ord k, Show k) => k -> Environment k v -> Err v
find i []    = fail $ show i ++ " wasn't found"
find i (f:e) =
  case Map.lookup i f of
    Just val -> return val
    Nothing -> find i e

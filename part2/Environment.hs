module Environment where

import qualified Data.Map as Map
import ErrM
import AbsCPP

type Frame = Map.Map Id Type
type Environment = [Frame]

newFrame :: Environment -> Environment
newFrame e = emptyFrame ++ e

deleteFrame :: Environment -> Environment
deleteFrame = tail

emptyFrame :: Environment
emptyFrame = [Map.empty]

-- FIXME: This should fail with an Err
update :: Id -> Type -> Environment -> Environment
update i v (f:e) =
  if Map.member i f
  then Map.insert i v f : e
  else f : update i v e
update _ _ [] = error "could not update"
--update i v [] = Bad $ "could not update id " ++ (show i) ++ 
--  " with type " ++ (show v)

add :: Id -> Type -> Environment -> Err Environment
add i v (f:e) =
  if Map.member i f
  then Bad $ show i ++ " already exists"
  else Ok $ Map.insert i v f : e
add _ _ [] = Bad "no environment found"

find :: Id -> Environment -> Err Type
find i []    = Bad $ show i ++ " wasn't found"
find i (f:e) = 
  case Map.lookup i f of
    Just val -> Ok val
    Nothing -> find i e



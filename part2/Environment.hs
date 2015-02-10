module Environment where

import qualified Data.Map as Map

type Environment id val = [Map.Map id val]
type Frame id val = Map.Map id val

newFrame :: Environment id val -> Environment id val
newFrame e = Map.empty : e

deleteFrame :: Environment id val -> Environment id val
deleteFrame = tail

empty :: Environment id val
empty = [Map.empty]

update :: (Ord id) => id -> val -> Environment id val -> Environment id val
update i v (f:e) =
  if Map.member i f
  then Map.insert i v f : e
  else f : update i v e
update _ _ [] = error "You're a tard"

add :: (Ord id) => id -> val -> Environment id val -> Environment id val
add i v (f:e) =
  if Map.member i f
  then error "UH NUH"
  else Map.insert i v f : e
add _ _ [] = error "Still a tard :("

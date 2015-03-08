{-# LANGUAGE RankNTypes #-}
module Interpreter where

import AbsCPP
import PrintCPP
import ErrM

import Control.Monad
import qualified Data.Map as M
import Data.IORef
import Data.Maybe

data Val = VInt Int deriving(Show)

interpret :: Program -> Bool -> Err Val
interpret (PDefs defs) _ = undefined
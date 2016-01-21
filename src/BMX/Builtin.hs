{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Builtin (
    defaultState
  , builtinHelpers
  , builtinDecorators
  ) where

import qualified Data.Map.Strict as M

import           BMX.Data
import           BMX.Builtin.Helpers (builtinHelpers)
import           BMX.Builtin.Decorators (builtinDecorators)

import           P

defaultState :: (Applicative m, Monad m) => Context -> EvalState m
defaultState c = EvalState {
    evalContext = [c]
  , evalData = M.empty -- FIX register @root = c
  , evalHelpers = builtinHelpers
  , evalPartials = M.empty -- FIX pass partials in too
  , evalDecorators = builtinDecorators
  }

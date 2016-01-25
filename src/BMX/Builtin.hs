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

-- | The default state: an empty context, all the helpers from
-- @BMX.Builtin.Helpers@, and all the decorators from
-- @BMX.Builtin.Decorators@.
defaultState :: (Applicative m, Monad m) => EvalState m
defaultState = mempty {
    evalHelpers = M.fromList builtinHelpers
  , evalDecorators = M.fromList builtinDecorators
  }

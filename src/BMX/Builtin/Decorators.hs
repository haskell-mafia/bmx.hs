{-| The collection of builtin decorators, included in the default environment. -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Builtin.Decorators where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

import           BMX.Data
import           BMX.Eval
import           BMX.Function

import           P

-- | The "inline" block decorator. Turns the block argument into a partial
-- with the name of the first argument.
inline :: (Applicative m, Monad m) => Decorator m
inline = BlockDecorator $ \block k -> do
  (StringV name) <- string
  liftBMX $ do
    let newPartial = Partial (eval block)
    withPartial name newPartial k

builtinDecorators :: (Applicative m, Monad m) => Map Text (Decorator m)
builtinDecorators = M.fromList [
    ("inline", inline)
  ]

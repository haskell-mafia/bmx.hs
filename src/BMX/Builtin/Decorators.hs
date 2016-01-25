{-| The collection of builtin decorators, included in the default environment. -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Builtin.Decorators where

import           Data.Text (Text)

import           BMX.Data
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

builtinDecorators :: (Applicative m, Monad m) => [(Text, Decorator m)]
builtinDecorators = [
    ("inline", inline)
  ]

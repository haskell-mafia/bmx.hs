{-| The collection of builtin helpers, included in the default environment. -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Helpers where

import qualified Data.Text as T

import           BMX.Data
import           BMX.Eval
import           BMX.Function

import           P

-- | The "noop" block helper. Discards both branches and returns mempty.
helper_noop :: (Applicative m, Monad m) => Helper m
helper_noop = BlockHelper $ \_ _ -> return mempty

-- | The "if" block helper.
helper_if :: (Applicative m, Monad m) => Helper m
helper_if = BlockHelper $ \thenp elsep -> do
  v <- value
  liftBMX . evalProgram $ if truthy v then thenp else elsep

-- | The "with" block helper. Accept a Context as argument.
helper_with :: (Applicative m, Monad m) => Helper m
helper_with = BlockHelper $ \thenp elsep -> do
  ctx <- optional context
  liftBMX $ maybe (evalProgram elsep)
                  (\c -> withContext c (evalProgram thenp))
                  ctx

-- | The "log" helper. Writes every argument to the log in a single line.
helper_log :: (Applicative m, Monad m) => Helper m
helper_log = Helper $ do
  args <- many value
  liftBMX $ do
    logs (T.unwords $ fmap renderValue args)
    return (StringV "")

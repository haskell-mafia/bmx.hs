{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Eval (
    evalProgram
  ) where

import qualified Data.Text as T

import BMX.Data

import P

evalProgram :: Monad m => Program -> BMX m Page
evalProgram (Program ss) = concatMapM evalStmt ss

evalStmt :: Monad m => Stmt -> BMX m Page
evalStmt s = case s of
  ContentStmt t -> return (content t)
  CommentStmt (Fmt l r) _ -> return (page l r T.empty)
  _ -> return mempty


{-

Helper / block helper:
  - If helper not found, warn and return mempty

Partial:
  - if partial not found, throw error

Partial block
  - if partial not found, fail over to rendering the other PRogram passed in

Decorator / block decorator:
  - always throw if not found

-}

-- -----------------------------------------------------------------------------
-- Util

concatMapM :: (Monad m, Monoid i) => (a -> m i) -> [a] -> m i
concatMapM f xs = liftM mconcat (mapM f xs)

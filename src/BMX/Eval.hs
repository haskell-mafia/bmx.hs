{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Eval (
    evalProgram
  ) where

import           Control.Monad.Reader hiding (mapM)
import qualified Data.Map as M
import qualified Data.Text as T

import BMX.Data
import BMX.Function

import P

evalProgram :: Monad m => Program -> BMX m Page
evalProgram (Program ss) = concatMapM evalStmt ss

evalStmt :: Monad m => Stmt -> BMX m Page
evalStmt = \case
  ContentStmt t -> return (content t)
  CommentStmt (Fmt l r) _ -> return (page l r T.empty)
  Mustache (Fmt l r) e -> liftM escapePage (evalMustache l r e)
  MustacheUnescaped (Fmt l r) e -> evalMustache l r e
  Block (Fmt l1 r1) (Fmt l2 r2) e bp b i -> evalBlock l1 r1 l2 r2 e bp b i
  Inverse (Fmt l r) p -> liftM ((page l r T.empty) <>) (evalProgram p)
  _ -> return mempty

evalExpr :: Monad m => Expr -> BMX m Value
evalExpr = \case
  (SExp h p hash) -> handleLit h p hash
  (Lit l) -> handleLit l [] mempty
  where
    handleLit :: Monad m => Literal -> [Expr] -> Hash -> BMX m Value
    handleLit l p hash = do
      help <- helperFromLit l
      vals <- mapM evalExpr p
      maybe
        (if null vals then valueLookup l else err (TypeError "helper" "value"))
        (withHash hash . runHelper vals)
        help
    --
    valueLookup l = do
      mv <- valueFromLit l
      maybe (warn (NoSuchValue (renderLiteral l)) >> return (StringV T.empty))
            return
            mv

evalMustache :: Monad m => Format -> Format -> Expr -> BMX m Page
evalMustache l r e = do
  ret <- evalExpr e
  return (page l r (renderValue ret))


evalBlock :: Monad m => Format -> Format -> Format -> Format
          -> Expr -> Maybe BlockParams -> Program -> (Maybe Stmt)
          -> BMX m Page
evalBlock l1 r1 l2 r2 e _bp block inverse = case e of
  Lit l -> do
    help <- helperFromLit l
    -- FIX blockparams?
    body <- maybe
              (err (NoSuchBlockHelper (renderLiteral l)))
              (runBlockHelper [] block pinverse)
              help
    -- Inner and outer formatting are both used
    return (page l1 r1 T.empty <> body <> page l2 r2 T.empty)
  SExp h p hash -> do
    help <- helperFromLit h
    args <- mapM evalExpr p
    body <- maybe
              (err (NoSuchBlockHelper (renderLiteral h)))
              (withHash hash . runBlockHelper args block pinverse)
              help
    -- Inner and outer formatting are both used
    return (page l1 r1 T.empty <> body <> page l2 r2 T.empty)
  where pinverse = maybe (Program []) (\i -> Program [i]) inverse
        -- FIX just store it as a Program in the AST


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

foldHashPairs :: Monad m => [HashPair] -> BMX m a -> BMX m a
foldHashPairs hps k = foldl' foldFun k hps
  where
    foldFun k' (HashPair key val) = do
      val' <- evalExpr val
      withVariable key val' k'

withHash :: Monad m => Hash -> BMX m a -> BMX m a
withHash (Hash hps) = foldHashPairs hps

helperFromLit :: Monad m => Literal -> BMX m (Maybe (Helper m))
helperFromLit = \case
  PathL p -> do
    help <- hlookup p
    return help
  _ -> err (TypeError "helper" "literal")

valueFromLit :: Monad m => Literal -> BMX m (Maybe Value)
valueFromLit = \case
  NullL -> val NullV
  UndefinedL -> val UndefinedV
  BooleanL b -> val (BoolV b)
  NumberL i -> val (IntV i)
  StringL s -> val (StringV s)
  PathL p -> vlookup p
  DataL p -> dlookup p
  where val = return . Just

-- FIX this doesn't belong here, but it doesn't fit in Data.Eval either
hlookup :: Monad m => Path -> BMX m (Maybe (Helper m))
hlookup p = do
  st <- ask
  return $ M.lookup (renderPath p) (evalHelpers st)

-- plookup
-- dlookup


-- -----------------------------------------------------------------------------
-- Util

concatMapM :: (Monad m, Monoid i) => (a -> m i) -> [a] -> m i
concatMapM f xs = liftM mconcat (mapM f xs)

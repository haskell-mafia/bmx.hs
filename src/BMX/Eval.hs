{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Eval (
    eval
  ) where

import           Control.Monad.Reader hiding (mapM)
import           Data.Text (Text)
import qualified Data.Text as T

import BMX.Data
import BMX.Function

import P

eval :: (Applicative m, Monad m) => Template -> BMX m Page
eval (Template ss) = foldDecorators ss (concatMapM evalStmt ss)

evalStmt :: (Applicative m, Monad m) => Stmt -> BMX m Page
evalStmt = \case
  -- Use the Formattee constructor
  ContentStmt t -> return (content t)
  -- An empty Page that performs formatting
  CommentStmt (Fmt l r) _ -> return (page l r T.empty)
  -- Evaluate and render the expression, escaping the output
  Mustache (Fmt l r) e -> liftM escapePage (evalMustache l r e)
  -- Evaluate and render the expression, without escaping
  MustacheUnescaped (Fmt l r) e -> evalMustache l r e
  -- Pass to handler that resolves and applies the named Helper
  Block (Fmt l1 r1) (Fmt l2 r2) e bp b i -> evalBlock l1 r1 l2 r2 e bp b i
  -- Evaluate the template fragment, and apply formatting to the head of it
  Inverse (Fmt l r) p -> liftM ((page l r T.empty) <>) (eval p)
  -- Treat this as a block with the 'then' and 'else' branches switched
  InverseBlock (Fmt l1 r1) (Fmt l2 r2) e bp b i -> evalBlock l1 r1 l2 r2 e bp i b
  -- Treat this as a block too, although it lacks the lower formatting
  InverseChain (Fmt l r) e bp b i -> evalBlock l r Verbatim Verbatim e bp b i
  -- Special handler that resolves and inlines the partial
  PartialStmt (Fmt l r) e ee ->
    evalPartial l Verbatim r Verbatim e ee (err . NoSuchPartial . renderLiteral)
  -- Special handler that registers @partial-block, and fails over if partial not found
  PartialBlock _ _ _ _ _ -> err (SomeError "Partial blockks unimplemented")
  -- PartialBlock (Fmt l1 r1) (Fmt l2 r2) e ee b -> evalPartialBlock l1 r1 l2 r2 e ee b
  RawBlock e body -> evalRawBlock e body
  -- Decorators are handled in a first pass, so here they are mere formatting
  DecoratorStmt (Fmt l r) _ -> return (page l r T.empty)
  DecoratorBlock (Fmt l _) (Fmt _ r) _ _ -> return (page l r T.empty)

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
        (if null p then valueLookup l else err (TypeError "helper" "value"))
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
          -> Expr -> BlockParams -> Template -> Template
          -> BMX m Page
evalBlock l1 r1 l2 r2 e _bp block inverse = case e of
  Lit l -> do
    help <- helperFromLit l
    -- FIX blockparams?
    body <- maybe
              (err (NoSuchBlockHelper (renderLiteral l)))
              (runBlockHelper [] block inverse)
              help
    -- Inner and outer formatting are both used
    return (page l1 r1 T.empty <> body <> page l2 r2 T.empty)
  SExp h p hash -> do
    help <- helperFromLit h
    args <- mapM evalExpr p
    body <- maybe
              (err (NoSuchBlockHelper (renderLiteral h)))
              (withHash hash . runBlockHelper args block inverse)
              help
    -- Inner and outer formatting are both used
    return (page l1 r1 T.empty <> body <> page l2 r2 T.empty)

evalPartial :: (Applicative m, Monad m) => Format -> Format -> Format -> Format
            -> Expr -> Maybe Expr -> (Literal -> BMX m Page) -> BMX m Page
evalPartial _l1 _r1 _l2 _r2 (SExp _e _p _hash) _extra _errf =
  err (SomeError "dynamic partials not implemented")
evalPartial l1 r1 l2 r2 (Lit p) extra errf = case extra of
  Just (Lit _) -> err (SomeError "parser failure (partial): Lit where SExpr should appear")
  Nothing -> do
    -- No hash or extra context
    part <- partialFromLit p
    body <- maybe (err (NoSuchPartial (renderLiteral p))) (runPartial []) part
    return (page l1 r1 T.empty <> body <> page l2 r2 T.empty)
  Just (SExp ctx ps hash) -> do
    -- Extra context and / or hash vals. ps should be empty, enforced by runPartial
    part <- partialFromLit p
    vals <- mapM evalExpr ps
    ctxv <- valueFromLit ctx
    ctx' <- case ctxv of
              Just (ContextV c) -> return c
              _ -> err (TypeError "context" "subexpression or value")
    body <- maybe (errf p)
                  (withContext ctx' . withHash hash . runPartial vals)
                  part
    return (page l1 r1 T.empty <> body <> page l2 r2 T.empty)

-- Need to fix Data definition first
-- evalPartialBlock l1 r1 l2 r2 e ee b = 

-- evalDecorator :: Monad m => Format -> Format -> Decorator -> BMX m Page -> BMX m Page

evalRawBlock :: Monad m => Expr -> Text -> BMX m Page
evalRawBlock e t = evalBlock
  Verbatim Verbatim Verbatim Verbatim
  e mempty (Template [ContentStmt t]) (Template [])

-- | Apply all Decorator statements, then run the continuation @k@.
foldDecorators :: Monad m => [Stmt] -> BMX m Page -> BMX m Page
foldDecorators sts k = foldl' foldFun k sts
  where
    nsd = err . NoSuchDecorator . renderLiteral
    --
    foldFun k' (DecoratorStmt _ (SExp e ps hash)) = do
      deco <- decoratorFromLit e
      vals <- mapM evalExpr ps
      maybe (nsd e) (\d -> withHash hash (withDecorator vals d k')) deco
    foldFun k' (DecoratorStmt _ (Lit e)) = do
      deco <- decoratorFromLit e
      maybe (nsd e) (\d -> withDecorator [] d k') deco
    --
    foldFun k' (DecoratorBlock _ _ (SExp e ps hash) block) = do
      deco <- decoratorFromLit e
      vals <- mapM evalExpr ps
      maybe (nsd e) (\d -> withHash hash (withBlockDecorator vals block d k')) deco
    foldFun k' (DecoratorBlock _ _ (Lit e) block) = do
      deco <- decoratorFromLit e
      maybe (nsd e) (\d -> withBlockDecorator [] block d k') deco
    --
    foldFun k' _ = k'

-- | Register each hashpair in the current context, then run a continuation.
foldHashPairs :: Monad m => [HashPair] -> BMX m a -> BMX m a
foldHashPairs hps k = foldl' foldFun k hps
  where
    foldFun k' (HashPair key val@(SExp _ _ _)) = do
      val' <- evalExpr val
      withVariable key val' k'
    foldFun k' (HashPair key (Lit l)) = do
      val' <- valueFromLit l
      maybe (err (ENoSuchValue (renderLiteral l)))
            (\v -> withVariable key v k')
            val'

withHash :: Monad m => Hash -> BMX m a -> BMX m a
withHash (Hash hps) = foldHashPairs hps

helperFromLit :: Monad m => Literal -> BMX m (Maybe (Helper m))
helperFromLit = \case
  PathL p -> do
    help <- lookupHelper p
    return help
  _ -> return Nothing

partialFromLit :: Monad m => Literal -> BMX m (Maybe (Partial m))
partialFromLit = \case
  PathL p -> lookupPartial p
  -- FIX Data variables not implemented properly - Partial is not a Value
  DataL _p -> err (SomeError "data partials not implemented") -- dlookup p
  _ -> err (TypeError "partial" "literal")

decoratorFromLit :: Monad m => Literal -> BMX m (Maybe (Decorator m))
decoratorFromLit = \case
  PathL p -> lookupDecorator p
  _ -> err (TypeError "decorator" "literal")

valueFromLit :: Monad m => Literal -> BMX m (Maybe Value)
valueFromLit = \case
  NullL -> val NullV
  UndefinedL -> val UndefinedV
  BooleanL b -> val (BoolV b)
  NumberL i -> val (IntV i)
  StringL s -> val (StringV s)
  PathL p -> lookupValue p
  DataL p -> lookupData p
  where val = return . Just

-- -----------------------------------------------------------------------------
-- Util

concatMapM :: (Monad m, Monoid i) => (a -> m i) -> [a] -> m i
concatMapM f xs = liftM mconcat (mapM f xs)

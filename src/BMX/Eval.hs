{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
module BMX.Eval (
    eval
  , partialFromTemplate
  , renderTemplate
  , renderTemplateM
  , renderTemplateIO
  ) where

import           Data.Functor.Identity (Identity)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.IO (IO)

import           BMX.Data
import           BMX.Eval.Function

import           P


-- -----------------------------------------------------------------------------
-- Public rendering interface

-- | Apply a 'Template' to some 'BMXState', producing a 'Page'.
--
-- All helpers, partials and decorators must be pure. Use 'renderTemplateIO'
-- if IO helpers are required.
renderTemplate :: BMXState Identity -> Template -> Either BMXError Page
renderTemplate bst t = do
  st <- packState bst
  first BMXEvalError $ runBMX st (eval t)

-- | Apply a 'Template' to a 'BMXState' in some Monad stack, producing
-- a 'Page'.
renderTemplateM :: (Applicative m, Monad m) => BMXState m -> Template -> m (Either BMXError Page)
renderTemplateM bst t = either (return . Left) runIt (packState bst)
  where runIt es = do
          ep <- runBMXT es (eval t)
          return (first BMXEvalError ep)

-- | Apply a 'Template' to some 'BMXState', producing a 'Page'.
--
-- Helpers, partials and decorators may involve IO. Use @renderTemplate@ if
-- no IO helpers are to be invoked.
renderTemplateIO :: BMXState IO -> Template -> IO (Either BMXError Page)
renderTemplateIO bst = renderTemplateM bst

-- | Produce a 'Partial' from an ordinary 'Template'.
partialFromTemplate :: (Applicative m, Monad m) => Template -> Partial m
partialFromTemplate = partial . eval


-- -----------------------------------------------------------------------------
-- Actual renderer

-- | Evaluate a 'Template' in the current 'BMX' environment, yielding a 'Page'.
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
  PartialStmt (Fmt l r) e ee hash ->
    evalPartial l Verbatim r Verbatim e ee hash (err . NoSuchPartial . renderLiteral)
  -- Special handler that registers @partial-block, and fails over if partial not found
  PartialBlock (Fmt l1 r1) (Fmt l2 r2) e ee hash b -> evalPartialBlock l1 r1 l2 r2 e ee hash b
  -- Special handler that treats it as a regular block with a single ContentStmt
  RawBlock e body -> evalRawBlock e body
  -- Decorators are handled in a first pass, so here they are mere formatting
  DecoratorStmt (Fmt l r) _ -> return (page l r T.empty)
  DecoratorBlock (Fmt l _) (Fmt _ r) _ _ -> return (page l r T.empty)

evalExpr :: Monad m => Expr -> BMX m Value
evalExpr = \case
  (SExp h p hash) -> evalExpr' True h p hash
  (Lit l) -> evalExpr' True l [] mempty

evalExpr' :: Monad m => Bool -> Literal -> [Expr] -> Hash -> BMX m Value
evalExpr' b l p _hash = do
  help <- helperFromLit l
  vals <- mapM evalExpr p
  maybe
    (if null p then valueLookupCoerce b l else err (TypeError "helper" "value"))
    (runHelper vals)
    help
  where
    valueLookupCoerce True ll = do
      mv <- valueFromLit ll
      -- We coerce undefined values to UndefinedV. We only tolerate this because
      -- the expression is an argument to a helper, not something we're rendering.
      -- e.g. the "if" helper relies on this behaviour.
      maybe (return UndefinedV) return mv
    valueLookupCoerce False ll = do
      mv <- valueFromLit ll
      -- Refuse to coerce - fail if not found.
      -- Could rely on evalMustache's 'render' check, but this provides a better error
      maybe (err (NoSuchValue (renderLiteral ll))) return mv

evalMustache :: Monad m => Format -> Format -> Expr -> BMX m Page
evalMustache l r = \case
  Lit _ -> err (ParserError "Lit found in Mustache")
  SExp lit ps hash -> do
    val <- evalExpr' False lit ps hash -- Do not allow coercion for lit
    render val
  where
    render p = liftM (page l r) $ case p of
      ContextV _ -> err (Unrenderable "context")
      ListV _ -> err (Unrenderable "list")
      UndefinedV -> err (Unrenderable "undefined")
      NullV -> err (Unrenderable "null")
      s@(StringV _) -> return (renderValue s)
      i@(NumberV _) -> return (renderValue i)
      b@(BoolV _) -> return (renderValue b)

evalBlock :: Monad m => Format -> Format -> Format -> Format
          -> Expr -> BlockParams -> Template -> Template
          -> BMX m Page
evalBlock l1 r1 l2 r2 e bp block inverse = case e of
  Lit l -> do
    help <- helperFromLit l
    body <- maybe
              (err (NoSuchHelper (renderLiteral l)))
              (runBlockHelper [] (toParams bp) block inverse)
              help
    -- Inner and outer formatting are both used. a block can strip its rendered contents
    return (page l1 r1 T.empty <> body <> page l2 r2 T.empty)
  SExp h p _hash -> do
    help <- helperFromLit h
    args <- mapM evalExpr p
    body <- maybe
              (err (NoSuchHelper (renderLiteral h)))
              (runBlockHelper args (toParams bp) block inverse)
              help
    -- Inner and outer formatting are both used
    return (page l1 r1 T.empty <> body <> page l2 r2 T.empty)

evalPartial :: (Applicative m, Monad m) => Format -> Format -> Format -> Format
            -> Expr -> Maybe Expr -> Hash -> (Literal -> BMX m Page) -> BMX m Page
evalPartial l1 r1 l2 r2 pp extra hash errf = case pp of
  -- Dynamic partial. Exp should eval to a string, then use that for a Partial lookup
  e@(SExp _ _ _) -> do
    val <- evalExpr e
    case val of
      StringV part -> if not (T.null part)
        then lookupPartial (PathID part Nothing) >>= maybe (errf (StringL part)) doPartial
        else errf (StringL part)
      v -> err (TypeError "string" (renderValueType v))
  -- Regular partial - look it right up alright
  Lit p -> partialFromLit p >>= maybe (errf p) doPartial
  where
    pFormat b = page l1 r1 T.empty <> b <> page l2 r2 T.empty
    --
    doPartial p = case extra of
      Nothing -> liftM pFormat (withHash hash (runPartial p))
      Just e -> do -- shift context
        parm <- evalExpr e
        maybe (err (TypeError "partial" (renderValueType parm)))
              (\c -> liftM pFormat (withContext c (withHash hash (runPartial p))))
              (ctxVal parm)
    --
    ctxVal = \case
      ContextV c -> Just c
      _ -> Nothing

evalPartialBlock :: (Applicative m, Monad m) => Format -> Format -> Format -> Format
                 -> Expr -> Maybe Expr -> Hash -> Template -> BMX m Page
evalPartialBlock l1 r1 l2 r2 e ee hash b =
  -- Register b as @partial-block
  -- Call evalPartial with custom error function (const (eval b)) - failover
  withData "partial-block" blockData (evalPartial l1 r1 l2 r2 e ee hash failOver)
  where
    blockData = DataPartial (partial (eval b))
    failOver = const (eval b)

-- | Evaluate a raw block.
evalRawBlock :: Monad m => Expr -> Text -> BMX m Page
evalRawBlock e t = evalBlock
  -- FIX Unsure if this approach is ok. Weird for BlockHelper to attack its block.
  -- Might be better to pack it as a StringV for a regular Helper, and expect a string.
  Verbatim Verbatim Verbatim Verbatim
  e mempty (Template [ContentStmt t]) (Template [])

-- | Apply all Decorator statements, then run the continuation @k@.
foldDecorators :: Monad m => [Stmt] -> BMX m Page -> BMX m Page
foldDecorators sts k = foldl' foldFun k sts
  where
    nsd = err . NoSuchDecorator . renderLiteral
    --
    foldFun k' (DecoratorStmt _ (SExp e ps _hash)) = do
      deco <- decoratorFromLit e
      vals <- mapM evalExpr ps
      maybe (nsd e) (\d -> withDecorator vals d k') deco
    foldFun k' (DecoratorStmt _ (Lit e)) = do
      deco <- decoratorFromLit e
      maybe (nsd e) (\d -> withDecorator [] d k') deco
    --
    foldFun k' (DecoratorBlock _ _ (SExp e ps _hash) block) = do
      deco <- decoratorFromLit e
      vals <- mapM evalExpr ps
      maybe (nsd e) (\d -> withBlockDecorator vals block d k') deco
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
      redefineVariable key val' k'
    foldFun k' (HashPair key (Lit l)) = do
      val' <- valueFromLit l
      maybe (err (NoSuchValue (renderLiteral l)))
            (\v -> redefineVariable key v k')
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
  DataL p -> do
    d <- lookupData p
    return $ case d of
      Just (DataPartial part) -> Just part
      _ -> Nothing
  _ -> err (TypeError "partial" "literal")

decoratorFromLit :: Monad m => Literal -> BMX m (Maybe (Decorator m))
decoratorFromLit = \case
  PathL p -> lookupDecorator p
  _ -> err (TypeError "decorator" "literal")

valueFromLit :: Monad m => Literal -> BMX m (Maybe Value)
valueFromLit = \case
  NullL -> val NullV
  BooleanL b -> val (BoolV b)
  NumberL i -> val (NumberV (realToFrac i))
  StringL s -> val (StringV s)
  PathL p -> lookupValueByPath p
  DataL p -> do
    md <- lookupData p
    return (md >>= dataVal)
  where
    val = return . Just
    dataVal = \case
      (DataValue v) -> Just v
      _ -> Nothing

toParams :: BlockParams -> [Param]
toParams (BlockParams ps) = fmap (Param . renderLiteral) ps

-- -----------------------------------------------------------------------------
-- Util

concatMapM :: (Monad m, Monoid i) => (a -> m i) -> [a] -> m i
concatMapM f xs = liftM mconcat (mapM f xs)

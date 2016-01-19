{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.Eval (
  -- * Evaluation state and abstract functions on it
    EvalState (..)
  , pushContext
  , withContext
  , withVariable
  , withPartial
  -- * Evaluation errors and results
  , EvalError (..)
  , renderEvalError
  , EvalOutput (..)
  , renderEvalOutput
  , EvalWarning (..)
  , renderEvalWarning
  -- * Evaluation monad
  , BMX
  , runBMX
  , runBMXIO
  , warn
  , err
  , logs
  , lookupValue
  , lookupData
  , lookupHelper
  , lookupPartial
  , lookupDecorator
  ) where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.DList (DList)
import qualified Data.DList as D
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Safe (headMay)
import           X.Control.Monad.Trans.Either

import           BMX.Data.AST
import           BMX.Data.Function
import           BMX.Data.Value

import           P


-- -----------------------------------------------------------------------------
-- BMX monad

-- | The main evaluation monad. Allows local changes to the state,
-- warnings, logs, and errors.
type BMX m = EitherT EvalError (StateT (DList EvalOutput) (ReaderT (EvalState m) m))

-- | Run a pure BMX action
runBMX :: EvalState Identity -> BMX Identity a -> (Either EvalError a, [EvalOutput])
runBMX st = fmap D.toList
  . runIdentity
  . (`runReaderT` st)
  . (`runStateT` mempty)
  . runEitherT

-- | Run a BMX action in an IO monad
runBMXIO :: MonadIO m => EvalState m -> BMX m a -> m (Either EvalError a, [EvalOutput])
runBMXIO st b = do
  (a, c) <- (`runReaderT` st) . (`runStateT` mempty) . runEitherT $ b
  return (a, D.toList c)

warn :: Monad m => EvalWarning -> BMX m ()
warn e = modify' (`D.snoc` (Warning e))

logs :: Monad m => Text -> BMX m ()
logs l = modify' (`D.snoc` (LogEntry l))

err :: Monad m => EvalError -> BMX m a
err = left

-- -----------------------------------------------------------------------------
-- Evaluation state

-- | EvalState holds the evaluation environment, i.e. all bound helpers,
-- partials, decorators, data variables, and the context stack.
data EvalState m = EvalState
  { -- | Stack of contexts, current on top.
    evalContext :: !([Context])
    -- | Special variables set by various things. See http://handlebarsjs.com/reference.html.
  , evalData :: !(Map Text Value)
    -- | All currently available helpers.
  , evalHelpers :: !(Map Text (HelperT (BMX m)))
    -- | All currently available partials.
  , evalPartials :: !(Map Text (PartialT (BMX m)))
    -- | All currently available decorators.
  , evalDecorators :: !(Map Text (DecoratorT (BMX m)))
  }

instance Monoid (EvalState m) where
  mempty = EvalState {
      evalContext = mempty
    , evalData = mempty
    , evalHelpers = mempty
    , evalPartials = mempty
    , evalDecorators = mempty
    }
  mappend !a !b = EvalState {
      evalContext = evalContext a <> evalContext b
    , evalData = evalData a `M.union` evalData b
    , evalHelpers = evalHelpers a `M.union` evalHelpers b
    , evalPartials = evalPartials a `M.union` evalPartials b
    , evalDecorators = evalDecorators a `M.union` evalDecorators b
    }

-- | Push a context to the top of the context stack.
pushContext :: Monad m => Context -> EvalState m -> EvalState m
pushContext !c es = es { evalContext = c : evalContext es }

-- | Replace the current context with another.
modifyContext :: Monad m => (Maybe Context -> Context) -> EvalState m -> EvalState m
modifyContext fun es =
  let newCtxs = case evalContext es of
        [] -> fun Nothing : []
        (x:xs) -> fun (Just x) : xs
  in es { evalContext = newCtxs }

-- | Push a new Context onto the stack for the duration of a single BMX action.
withContext :: Monad m => Context -> BMX m a -> BMX m a
withContext !c = local (pushContext c)

-- | Register a variable in the current context for one action.
withVariable :: Monad m => Text -> Value -> BMX m a -> BMX m a
withVariable key val k = shadowWarning >> local (modifyContext putVar) k
  where
    shadowWarning = do
      mv <- lookupValue (PathID key Nothing)
      maybe (return ()) (const $ warn (ShadowValue key)) mv
    --
    putVar Nothing = Context $ M.insert key val M.empty
    putVar (Just (Context ctx)) = Context $ M.insert key val ctx

-- | Register a partial in the current context for one action.
withPartial :: Monad m => Text -> PartialT (BMX m) -> BMX m a -> BMX m a
withPartial name p k = shadowWarning >> local addPartial k
  where
    shadowWarning = do
      mv <- lookupPartial (PathID name Nothing)
      maybe (return ()) (const $ warn (ShadowPartial name)) mv
    --
    addPartial es = es { evalPartials = M.insert name p (evalPartials es) }

-- | Look up a variable in the current context.
lookupValue :: Monad m => Path -> BMX m (Maybe Value)
lookupValue i = ask >>= (go i . evalContext)
  where
    -- Paths are allowed to start with parent / local references
    go _ [] = return Nothing
    go p (x:xs) = case (vname p, vrest p) of
      (".", Nothing) -> return (Just (ContextV x))
      ("..", Nothing) -> return (ContextV <$> headMay xs)
      (".", Just (_, p')) -> going p' x
      ("..", Just (_, p')) -> maybe (return Nothing) (going p') (headMay xs)
      _ -> going p x
    -- Traverse the rest of the path
    going p ctx = case (vname p, vrest p) of
      (t@".", _) -> err (InvalidPath t)
      (t@"..", _) -> err (InvalidPath t)
      (t, Nothing) -> return (resolve t ctx)
      (t, Just (_, p')) -> maybe (return Nothing) (\(ContextV c) -> going p' c) (resolve t ctx)
    --
    resolve t (Context c) = M.lookup t c
    --
    vname = \case
      PathID t _ -> t
      PathSeg t _ -> t
    --
    vrest = \case
      PathID _ r -> r
      PathSeg _ r -> r

-- | Look up a @data variable in the current context.
lookupData :: Monad m => DataPath -> BMX m (Maybe Value)
lookupData (DataPath p) = ask >>= \es ->
  let d = evalData es in case p of
    PathID t Nothing -> return (M.lookup t d)
    PathSeg t Nothing -> return (M.lookup t d)
    _ -> return Nothing

lookupHelper :: Monad m => Path -> BMX m (Maybe (HelperT (BMX m)))
lookupHelper p = do
  st <- ask
  return $ M.lookup (renderPath p) (evalHelpers st)

lookupPartial :: Monad m => Path -> BMX m (Maybe (PartialT (BMX m)))
lookupPartial p = do
  st <- ask
  return $ M.lookup (renderPath p) (evalPartials st)

lookupDecorator :: Monad m => Path -> BMX m (Maybe (DecoratorT (BMX m)))
lookupDecorator p = do
  st <- ask
  return $ M.lookup (renderPath p) (evalDecorators st)

-- -----------------------------------------------------------------------------
-- Evaluation errors and warnings

data EvalError
  = TypeError Text Text
  | HelperError FunctionError
  | PartialError FunctionError
  | DecoratorError FunctionError
  | InvalidPath Text
  | NoSuchPartial Text
  | NoSuchDecorator Text
  | NoSuchBlockHelper Text
  | ENoSuchValue Text
  | SomeError Text

data EvalOutput
  = Warning EvalWarning
  | LogEntry Text

data EvalWarning
  = WarnDNE Text
  | NoSuchHelper Text
  | NoSuchValue Text
  | ShadowValue Text
  | ShadowPartial Text

renderEvalError :: EvalError -> Text
renderEvalError = \case
  TypeError !e !a -> "Type error (expected " <> e <> ", actually " <> a <> ")"
  HelperError !fe -> "Helper misuse: " <> renderFunctionError fe
  PartialError !fe -> "Partial misuse: " <> renderFunctionError fe
  DecoratorError !fe -> "Decorator misuse: " <> renderFunctionError fe
  InvalidPath !t -> "Invalid path (" <> T.pack (show t) <> " can only appear at the start of a path)"
  NoSuchPartial !t -> "Partial " <> t <> " is not defined"
  NoSuchDecorator !t -> "Decorator " <> t <> " is not defined"
  NoSuchBlockHelper !t -> "Block helper " <> t <> " is not defined"
  ENoSuchValue !t -> "Value " <> t <> " is not defined"
  SomeError !t -> "Error: " <> t

renderEvalOutput :: EvalOutput -> Text
renderEvalOutput = \case
  Warning !ew -> "Warning: " <> renderEvalWarning ew
  LogEntry !t -> "Log: " <> t

renderEvalWarning :: EvalWarning -> Text
renderEvalWarning = \case
  WarnDNE !t -> t <> " does not exist"
  NoSuchHelper !t -> "Helper " <> t <> " is not defined"
  NoSuchValue !t -> "Value " <> t <> " is not defined"
  ShadowValue !t -> "The local definition of value " <> t <> " shadows an existing binding"
  ShadowPartial !t -> "The local definition of partial " <> t <> " shadows an existing binding"

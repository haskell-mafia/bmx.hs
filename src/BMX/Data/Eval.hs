{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module BMX.Data.Eval (
  -- * Evaluation state and abstract functions on it
    EvalState (..)
  , pushContext
  , withContext
  -- * Evaluation errors and results
  , EvalError (..)
  , renderEvalError
  , EvalOutput (..)
  , renderEvalOutput
  , EvalWarning (..)
  , renderEvalWarning
  -- * Specialised RW monad for evaluation
  , BMX
  , runBMX
  , runBMXIO
  , warn
  , err
  , logs
  ) where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.DList (DList)
import qualified Data.DList as D
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import           X.Control.Monad.Trans.Either

import           BMX.Data.Function
import           BMX.Data.Value

import           P


-- -----------------------------------------------------------------------------
-- BMX monad

-- | The main evaluation monad. Allows local changes to the state,
-- warnings, logs, and errors.
type BMX m = EitherT EvalError (StateT (DList EvalOutput) (ReaderT (EvalState m) m))

-- | Run a pure BMX action
runBMX :: BMX Identity a -> (Either EvalError a, [EvalOutput])
runBMX = fmap D.toList
  . runIdentity
  . (`runReaderT` mempty) -- FIX user should pass env in
  . (`runStateT` mempty)
  . runEitherT

-- | Run a BMX action in an IO monad
runBMXIO :: MonadIO m => BMX m a -> m (Either EvalError a, [EvalOutput])
runBMXIO b = do
  (a, c) <- (`runReaderT` mempty) . (`runStateT` mempty) . runEitherT $ b
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
  , evalHelpers :: !(Map Text (HelperT m))
    -- | All currently available partials.
  , evalPartials :: !(Map Text (PartialT m))
    -- | All currently available decorators.
  , evalDecorators :: !(Map Text (DecoratorT m))
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

-- | Push a new Context onto the stack for the duration of a single BMX action.
withContext :: Monad m => Context -> BMX m a -> BMX m a
withContext !c = local (pushContext c)

{-
vlookup :: Path -> BMX m (Maybe Value)
vlookup = \case
  Path pcs -> reader (gop pcs)
  DataPath pcs -> reader (datap pcs)
  where
    gop :: [PathComponents] -> EvalState -> Maybe Value
    gop _ (evalContext -> []) = Nothing
    gop [] _ = Nothing

    gop (PathID t : []) (evalContext -> ((Context ctx): _)) = M.lookup t ctx
    gop (SegmentID t : []) (evalContext -> ((Context ctx): _)) = M.lookup t ctx
-}

-- -----------------------------------------------------------------------------
-- Evaluation errors and warnings

data EvalError
  = TypeError Text Text
  | HelperError FunctionError
  | PartialError FunctionError
  | DecoratorError FunctionError

data EvalOutput
  = Warning EvalWarning
  | LogEntry Text

data EvalWarning
  = WarnDNE Text

renderEvalError :: EvalError -> Text
renderEvalError = \case
  TypeError !e !a -> "Type error (expected " <> e <> ", actually " <> a <> ")"
  HelperError !fe -> "Helper misuse: " <> renderFunctionError fe
  PartialError !fe -> "Partial misuse: " <> renderFunctionError fe
  DecoratorError !fe -> "Decorator misuse: " <> renderFunctionError fe

renderEvalOutput :: EvalOutput -> Text
renderEvalOutput = \case
  Warning !ew -> "Warning: " <> renderEvalWarning ew
  LogEntry !t -> "Log: " <> t

renderEvalWarning :: EvalWarning -> Text
renderEvalWarning = \case
  WarnDNE !t -> t <> "does not exist"

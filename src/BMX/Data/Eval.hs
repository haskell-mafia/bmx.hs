{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK not-home #-}
module BMX.Data.Eval (
  -- * Evaluation state and abstract functions over it
    EvalState (..)
  , pushContext
  , withContext
  , withVariable
  , redefineVariable
  , withData
  , withPartial
  , lookupValue
  , lookupData
  , lookupHelper
  , lookupPartial
  , lookupDecorator
  -- * Abstract user-facing types for Partial, Decorator, Helper
  , Helper (..)
  , helper
  , blockHelper
  , Partial (..)
  , partial
  , Decorator (..)
  , decorator
  , blockDecorator
  -- * Data variables
  , DataVar (..)
  -- * Evaluation errors and results
  , EvalError (..)
  , renderEvalError
  -- * Evaluation monad
  , BMX (..)
  , runBMX
  , runBMXT
  , err
  , bmxError
  ) where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Safe (atMay, headMay, readMay)
import           X.Control.Monad.Trans.Either

import           BMX.Data.AST
import           BMX.Data.Function
import           BMX.Data.Page
import           BMX.Data.Value

import           P


-- -----------------------------------------------------------------------------
-- BMX monad

-- | The main evaluation monad. Allows local changes to the state and fatal errors.
newtype BMX m a = BMX { bmxT :: EitherT EvalError (ReaderT (EvalState m) m) a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans BMX where
  lift = BMX . lift . lift

instance MonadIO m => MonadIO (BMX m) where
  liftIO = BMX . liftIO

-- | Run a pure BMX action
runBMX :: EvalState Identity -> BMX Identity a -> Either EvalError a
runBMX st = runIdentity . runBMXT st

-- | Run a BMX action in some monad stack
runBMXT :: Monad m => EvalState m -> BMX m a -> m (Either EvalError a)
runBMXT st = (`runReaderT` st) . runEitherT . bmxT

err :: Monad m => EvalError -> BMX m a
err = BMX . left

-- -----------------------------------------------------------------------------
-- User-facing abstract types for Helper, Partial and Decorator, plus
-- constructor functions.

-- | Functions that produce either a 'Value' or a 'Page', after accepting
-- 'Value' arguments and accessing local state.
newtype Helper m = Helper { unHelper :: HelperT (BMX m) }

-- | Functions that can make arbitrary changes to the local state.
newtype Decorator m = Decorator { unDecorator :: DecoratorT (BMX m) }

-- | An object that produces a 'Page', for a 'Template' to render inline.
newtype Partial m = Partial { unPartial :: PartialT (BMX m) }

-- | Construct a regular 'Helper' out of a 'FunctionT' action. Regular
-- helpers yield a 'Value'.
helper :: Monad m => FunctionT (BMX m) Value -> Helper m
helper = Helper . HelperT

-- | Construct a block 'Helper' from a binary function that handles
-- two 'Templates'. Block helpers yield a 'Page', typically (though
-- not necessarily) by calling 'eval' on one of their 'Template'
-- arguments.
blockHelper :: Monad m => (Template -> Template -> FunctionT (BMX m) Page) -> Helper m
blockHelper = Helper . BlockHelperT

-- | Construct a regular 'Decorator' from a function accepting a continuation
--  and yielding a 'Page'.
--
-- Decorators are specified in continuation-passing style. If the
-- continuation is discarded, the the block will not be
-- rendered. For example, this "abort" decorator will replace the
-- surrounding Template with an empty 'Page':
--
-- > abort :: (Applicative m, Monad m) => Decorator m
-- > abort = decorator $ \_k -> return mempty
decorator :: Monad m => (BMX m Page -> FunctionT (BMX m) Page) -> Decorator m
decorator = Decorator . DecoratorT

-- | Construct a block 'Decorator' from a function accepting a 'Template' block
-- and a continuation, yielding a 'Page'.
--
-- As per 'decorator', all decorators are specified in continuation-passing style.
-- If the continuation is discarded, the block will not be rendered.
--
-- The 'Template' argument can be used however one pleases. For
-- example, this decorator replaces the whole block it was called in with
-- its 'Template' argument if the variable @cool@ is not found:
--
-- > failoverCool :: (Applicative m, Monad m) => Decorator m
-- > failoverCool = blockDecorator $ \body k ->
-- >   liftBMX $ do
-- >     mv <- lookupValue "cool"
-- >     maybe (eval body) k mv
--
-- Running it on an empty context with this 'Template' will eradicate surrounding elements, returning only @" not cool! "@:
--
-- >>> ... abcde {{#* failoverCool }} not cool! {{/failoverCool}} ... {{abcde}}
-- " not cool! "
--
-- See the implementation of 'BMX.Builtin.Decorators.inline' for another example.
blockDecorator :: Monad m => (Template -> BMX m Page -> FunctionT (BMX m) Page) -> Decorator m
blockDecorator = Decorator . BlockDecoratorT

-- | Construct a 'Partial' from some 'BMX' monadic action that yields a 'Page'.
-- Most partials will be constructed with 'partialFromTemplate'. However,
-- the following is a valid partial:
--
-- > emptyPage :: (Applicative m, Monad m) => Partial m
-- > emptyPage = partial (return mempty)
--
-- Likewise, one could construct an IO partial if a good excuse could
-- be procured.
partial :: Monad m => BMX m Page -> Partial m
partial = Partial . PartialT

-- -----------------------------------------------------------------------------
-- Data variables (special vars prefixed with an @)

-- | Data variables. Helpers and Decorators can register data variables to
-- be invoked by some 'Template'.
--
-- For example, the builtin helper 'each' sets @\@first@ and @\@last@ to @True@
-- when on the first / last iteration.
--
-- Likewise, when invoking a partial block, the data variable @\@partial-block@
-- contains the block argument (in the form of a 'Partial').
data DataVar m
  = DataValue Value
  | DataPartial (Partial m)
  | DataHelper (Helper m)
  | DataDecorator (Decorator m)

-- -----------------------------------------------------------------------------
-- Evaluation state

-- | EvalState holds the rendering environment, i.e. all bound helpers,
-- partials, decorators, data variables, and the current variable context.
--
-- The type parameter @m@ refers to the base monad for the registered
-- helpers, partials and decorators. It is commonly 'Identity' or 'IO',
-- though a Template can be rendered on top of any monad stack.
data EvalState m = EvalState
  { -- | Stack of contexts, current on top.
    evalContext :: !([Context])
    -- | Special variables set by various things. Can be values, partials, etc.
    -- See http://handlebarsjs.com/reference.html.
  , evalData :: !(Map Text (DataVar m))
    -- | All currently available helpers.
  , evalHelpers :: !(Map Text (Helper m))
    -- | All currently available partials.
  , evalPartials :: !(Map Text (Partial m))
    -- | All currently available decorators.
  , evalDecorators :: !(Map Text (Decorator m))
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

-- | Push a new 'Context' onto the stack, then run some action in the 'BMX' monad.
--
-- The 'Context' is changed only for the duration of that action.
withContext :: Monad m => Context -- ^ The new top-level 'Context'
            -> BMX m a -- ^ The action to run with modified 'Context'
            -> BMX m a
withContext !c b = BMX $ local (pushContext c) (bmxT b)

-- | Register a variable in the current context, then run some action
-- in the 'BMX' monad.
--
-- The 'Context' is only changed for the duration of that action.
--
-- Redefinition of existing variables is not permitted, and will throw an error.
withVariable :: Monad m => Text -- ^ The name to be bound
             -> Value -- ^ The value the binding should point to
             -> BMX m a -- ^ The action to run with modified 'Context'
             -> BMX m a
withVariable key UndefinedV _ = err (DefUndef key)
withVariable key val k = noShadowing >> redefineVariable key val k
  where
    noShadowing = do
      mv <- lookupValue (PathID key Nothing)
      maybe (return ()) (const $ err (ShadowValue key)) mv

-- | Register a variable in the current context, then run some action
-- in the 'BMX' monad.
--
-- The 'Context' is only changed for the duration of that action.
--
-- WARNING: Redefinition of existing variables is permitted
redefineVariable ::
     Monad m
  => Text -- ^ The name to be bound
  -> Value -- ^ The value the binding should point to
  -> BMX m a -- ^ The action to run with modified 'Context'
  -> BMX m a
redefineVariable key UndefinedV _ = err (DefUndef key)
redefineVariable key val k =
  BMX (local (modifyContext putVar) (bmxT k))
  where
    --
    putVar Nothing = Context $ M.insert key val M.empty
    putVar (Just (Context ctx)) = Context $ M.insert key val ctx

-- | Register a data variable in the current context, then run some
-- action in the 'BMX' monad.
--
-- The new 'DataVar' will persist only for the duration of that action.
withData :: Monad m => Text -- ^ The name to be bound. Note that the @\@@ is implicit
         -> DataVar m -- ^ The 'DataVar' the binding should point to
         -> BMX m a -- ^ The action to run with modified environment
         -> BMX m a
withData key val k = BMX (local addData (bmxT k))
  where
    addData es = es { evalData = M.insert key val (evalData es) }

-- | Register a partial in the current context, then run some action
-- in the 'BMX' monad.
--
-- The new 'Partial' will persist only for the duration of that action.
--
-- Redefinition of existing partials is not permitted, and will throw an error.
withPartial :: Monad m => Text -- ^ The name to be bound
            -> Partial m -- ^ The 'Partial' the binding should point to
            -> BMX m a -- ^ The action to run with modified environment
            -> BMX m a
withPartial name p k = noShadowing >> BMX (local addPartial (bmxT k))
  where
    noShadowing = do
      mv <- lookupPartial (PathID name Nothing)
      maybe (return ()) (const $ err (ShadowPartial name)) mv
    --
    addPartial es = es { evalPartials = M.insert name p (evalPartials es) }

-- | Look up a 'Value' in the current 'Context'.
lookupValue :: Monad m => Path -> BMX m (Maybe Value)
-- FIX replace Path with some public type - probably Text
lookupValue i = BMX $ ask >>= (bmxT . go i . evalContext)
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
      (t, Just (_, p')) -> maybe (return Nothing) (step p') (resolve t ctx)
    --
    goingl p l = case (vname p, vrest p) of
      (t, Just (_, p')) -> maybe (return Nothing) (step p') (resolvel t l)
      (t, Nothing) -> return (resolvel t l)
    --
    resolve t (Context c) = M.lookup t c
    resolvel t ls = maybe Nothing (atMay ls) (readMay (T.unpack t))
    --
    step p = \case
      ContextV c -> going p c
      ListV l -> goingl p l
      _ -> return Nothing
    --
    vname = \case
      PathID t _ -> t
      PathSeg t _ -> t
    --
    vrest = \case
      PathID _ r -> r
      PathSeg _ r -> r

-- | Look up a 'DataVar' in the current environment.
lookupData :: Monad m => DataPath -> BMX m (Maybe (DataVar m))
lookupData (DataPath p) = BMX $ ask >>= \es -> bmxT $
  let d = evalData es in case p of
    PathID t Nothing -> return (M.lookup t d)
    PathSeg t Nothing -> return (M.lookup t d)
    _ -> return Nothing

-- | Look up a 'Helper' in the current environment.
lookupHelper :: Monad m => Path -> BMX m (Maybe (Helper m))
lookupHelper p = BMX $ do
  st <- ask
  return $ M.lookup (renderPath p) (evalHelpers st)

-- | Look up a 'Partial' in the current environment.
lookupPartial :: Monad m => Path -> BMX m (Maybe (Partial m))
lookupPartial p = BMX $ do
  st <- ask
  return $ M.lookup (renderPath p) (evalPartials st)

-- | Look up a 'Decorator' in the current environment.
lookupDecorator :: Monad m => Path -> BMX m (Maybe (Decorator m))
lookupDecorator p = BMX $ do
  st <- ask
  return $ M.lookup (renderPath p) (evalDecorators st)

-- -----------------------------------------------------------------------------
-- Evaluation errors and warnings

data EvalError
  = TypeError !Text !Text -- ^ A type error, with "expected" and "actual" fields.
  | HelperError !FunctionError -- ^ Incorrect arguments for a Helper.
  | PartialError !FunctionError -- ^ Incorrect arguments for a Partial.
  | DecoratorError !FunctionError -- ^ Incorrect arguments for a Decorator.
  | InvalidPath !Text -- ^ A traversed Path had invalid format - likely "." or ".." misuse.
  | NoSuchPartial !Text -- ^ An invoked partial was not found, and there was no failover template.
  | NoSuchDecorator !Text -- ^ An invoked decorator was not found.
  | NoSuchHelper !Text -- ^ An invoked block helper was not found.
  | NoSuchValue !Text -- ^ A variable was not found, and it was unsafe to proceed.
  | ParserError !Text -- ^ An absurd case - indicative of an error in the parser.
  | Unrenderable !Text -- ^ Attempt to render an undefined, list or context.
  | ShadowValue !Text -- ^ Attempt to redefine a variable in the current context
  | ShadowPartial !Text -- ^ Attempt to redefine a partial
  | ShadowHelper !Text -- ^ Attempt to redefine a helper
  | ShadowDecorator !Text -- ^ Attempt to redefine a Decorator
  | DefUndef !Text -- ^ Attempt to define a variable as 'undefined' (using withVariable)
  | UserError !Text -- ^ Custom error thrown from a helper.

renderEvalError :: EvalError -> Text
renderEvalError = \case
  TypeError e a -> "Type error (expected " <> e <> ", actually " <> a <> ")"
  HelperError fe -> "Helper misuse: " <> renderFunctionError fe
  PartialError fe -> "Partial misuse: " <> renderFunctionError fe
  DecoratorError fe -> "Decorator misuse: " <> renderFunctionError fe
  InvalidPath t -> "Invalid path (" <> T.pack (show t) <> " can only appear at the start of a path)"
  NoSuchPartial t -> "Partial '" <> t <> "' is not defined"
  NoSuchDecorator t -> "Decorator '" <> t <> "' is not defined"
  NoSuchHelper t -> "Helper '" <> t <> "' is not defined"
  NoSuchValue t -> "Value '" <> t <> "' is not defined"
  ParserError t -> "Parser error: " <> t
  Unrenderable t -> "Invalid mustache: cannot render '" <> t <> "'"
  ShadowValue t -> "The local definition of value '" <> t <> "' shadows an existing binding"
  ShadowHelper t -> "The local definition of helper '" <> t <> "' shadows an existing binding"
  ShadowPartial t -> "The local definition of partial '" <> t <> "' shadows an existing binding"
  ShadowDecorator t -> "The local definition of decorator '" <> t <> "' shadows an existing binding"
  DefUndef t -> "Attempt to define variable '" <> t <> "' as 'undefined' - no"
  UserError t -> "Error thrown in user code: " <> t

-- | Throw an error inside a 'BMX' action, with custom message.
bmxError :: Monad m => Text -> BMX m a
bmxError = err . UserError

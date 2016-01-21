{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.Function (
  -- * Argument-parsing transformer
    FunctionT
  , runFunctionT
  , liftBMX
  , one
  , try
  , param
  -- * Function arity and type errors
  , FunctionError
  , renderFunctionError
  -- * The various types of function
  , HelperT (..)
  , PartialT (..)
  , DecoratorT (..)
  ) where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Text (Text)
import qualified Data.Text as T
import           X.Control.Monad.Trans.Either

import           BMX.Data.AST
import           BMX.Data.Page
import           BMX.Data.Value

import           P

-- -----------------------------------------------------------------------------
-- FunctionT

-- | The FunctionT transformer adds va_arg-style argument list parsing to a monad stack.
-- We get a nice Applicative interface and only need to define two abstract combinators.
-- All Helpers and Decorators will be defined in terms of FunctionT.
type FunctionT m = EitherT FunctionError (StateT FunctionState m)

data FunctionState = FS [Value] [Param]

data FunctionError
  = Mismatch !Text !Text
  | Trailing !Int
  | EOF
  | NoParams

instance Monoid FunctionError where
  mempty = EOF
  mappend _ b = b

-- | Run the arg parser and return the rest of the computation in the inner monad.
runFunctionT :: Monad m => [Value] -> [Param] -> FunctionT m a -> m (Either FunctionError a)
runFunctionT s p f = do
  (a, _) <- runFunctionT' (FS s p) (do a <- f; _ <- eof; return a) -- f <* eof
  return a

runFunctionT' :: Monad m => FunctionState -> FunctionT m a -> m (Either FunctionError a, FunctionState)
runFunctionT' s = (flip runStateT) s . runEitherT

-- | Lift a BMX action into FunctionT.
liftBMX :: (Monad m, Monad (t m), MonadTrans t) => t m a -> FunctionT (t m) a
liftBMX = lift . lift

one :: Monad m => Text -> (Value -> Bool) -> FunctionT m Value
one rule p = do
  (FS st ps) <- get
  case st of (x:xs) -> if p x then (put (FS xs ps)) >> return x
                              else left (Mismatch rule (renderValueType x))
             _      -> left EOF

eof :: Monad m => FunctionT m ()
eof = do
  (FS st _) <- get
  if null st then return () else left (Trailing (length st))

try :: Monad m => FunctionT m a -> FunctionT m a
try p = do
  st <- get
  (r, st') <- (lift . lift) (runFunctionT' st p)
  case r of
    Left e -> left e
    Right v -> put st' >> return v

-- | Grab one block parameter.
param :: Monad m => FunctionT m Param
param = do
  (FS st ps) <- get
  case ps of (x:xs) -> put (FS st xs) >> return x
             _ -> left NoParams

renderFunctionError :: FunctionError -> Text
renderFunctionError = \case
  Mismatch e a -> "Type mismatch (expected " <> e <> ", got " <> a <> ")"
  Trailing i -> "Too many arguments (" <> T.pack (show i) <> " unused)"
  EOF -> "Not enough arguments"
  NoParams -> "Not enough block parameters"

-- -----------------------------------------------------------------------------
-- Functions (helpers, partials, decorators)

data HelperT m
  -- | Helpers are straightforward local operations on the BMX state, yielding a value
  = Helper (FunctionT m Value)
  -- | Block helpers render a page from a main branch and an inverse (else)
  | BlockHelper (Template -> Template -> FunctionT m Page)

data PartialT m
  -- | Partials are just BMX actions producing a Page.
  -- In practice, they will probably be lazy, fully-applied calls to eval.
  -- Partials invoked as blocks can access a template fragment, @partial-block.
  = Partial (m Page)

data DecoratorT m
  -- | Decorators make local changes to the BMX state, and accept a continuation
  = Decorator (m Page -> FunctionT m Page)
  -- | Block decorators accept a template fragment and a continuation
  | BlockDecorator (Template -> m Page -> FunctionT m Page)

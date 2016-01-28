{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.Function (
  -- * Argument-parsing transformer
    FunctionT
  , runFunctionT
  , liftBMX
  , one
  , param
  -- * Function arity and type errors
  , FunctionError (..)
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
newtype FunctionT m a = FunctionT { fun :: EitherT FunctionError (StateT FunctionState m) a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans FunctionT where
  lift = FunctionT . lift . lift

instance (Applicative m, Monad m) => Alternative (FunctionT m) where
  empty = FunctionT (left EOF)
  a <|> b = do -- try a <|> b
    st <- FunctionT get
    (r, st') <- (FunctionT . lift . lift) (runFunctionT' st a)
    case r of
      Left _ -> b
      Right v -> FunctionT (put st' >> return v)

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
  (a, _) <- runFunctionT' (FS s p) (do a <- f; _ <- eof; return a) -- try f <* eof
  return a

runFunctionT' :: Monad m => FunctionState -> FunctionT m a -> m (Either FunctionError a, FunctionState)
runFunctionT' s = (flip runStateT) s . runEitherT . fun

-- | Lift an action of type @BMX m a@ action into 'FunctionT'.
liftBMX :: (Monad m, Monad (t m), MonadTrans t) => t m a -> FunctionT (t m) a
liftBMX = FunctionT . lift . lift

one :: Monad m => Text -> (Value -> Bool) -> FunctionT m Value
one rule p = do
  (FS st ps) <- FunctionT get
  case st of (x:xs) -> if p x then FunctionT $ (put (FS xs ps)) >> return x
                              else (FunctionT . left) (Mismatch rule (renderValueType x))
             _      -> (FunctionT . left) EOF

eof :: Monad m => FunctionT m ()
eof = do
  (FS st _) <- FunctionT get
  if null st then return () else (FunctionT . left) (Trailing (length st))

-- | Grab one block parameter.
param :: Monad m => FunctionT m Param
param = do
  (FS st ps) <- FunctionT get
  case ps of (x:xs) -> FunctionT $ put (FS st xs) >> return x
             _ -> (FunctionT . left) NoParams

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
  = HelperT (FunctionT m Value)
  -- | Block helpers render a page from a main branch and an inverse (else)
  | BlockHelperT (Template -> Template -> FunctionT m Page)

data PartialT m
  -- | Partials are just BMX actions producing a Page.
  -- In practice, they will probably be lazy, fully-applied calls to eval.
  -- Partials invoked as blocks can access a template fragment, @partial-block.
  = PartialT (m Page)

data DecoratorT m
  -- | Decorators make local changes to the BMX state, and accept a continuation
  = DecoratorT (m Page -> FunctionT m Page)
  -- | Block decorators accept a template fragment and a continuation
  | BlockDecoratorT (Template -> m Page -> FunctionT m Page)

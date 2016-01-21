{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Function (
    Helper
  , Decorator
  , Partial
  -- *
  , value
  , string
  , number
  , boolean
  , nullv
  , undef
  , context
  , list
  -- *
  , runHelper
  , runBlockHelper
  , runPartial
  , withDecorator
  , withBlockDecorator
  ) where

import           BMX.Data

import           P

type Helper m = HelperT (BMX m)
type Decorator m = DecoratorT (BMX m)
type Partial m = PartialT (BMX m)

-- -----------------------------------------------------------------------------
-- Argument parsers for helpers / decorators

value :: Monad m => FunctionT m Value
value = one "value" (const True)

string :: Monad m => FunctionT m Value
string = one "string" isString
  where isString (StringV _) = True
        isString _ = False

number :: Monad m => FunctionT m Value
number = one "number" isNum
  where isNum (IntV _) = True
        isNum _ = False

boolean :: Monad m => FunctionT m Value
boolean = one "boolean" isBool
  where isBool (BoolV _) = True
        isBool _ = False

nullv :: Monad m => FunctionT m Value
nullv = one "null" isNull
  where isNull NullV = True
        isNull _ = False

undef :: Monad m => FunctionT m Value
undef = one "undefined" isUndef
  where isUndef UndefinedV = True
        isUndef _ = False

context :: Monad m => FunctionT m Value
context = one "context" isContext
  where isContext (ContextV _) = True
        isContext _ = False

list :: Monad m => FunctionT m Value
list = one "list" isList
  where isList (ListV _) = True
        isList _ = False

-- -----------------------------------------------------------------------------
-- Running / using a helper, partial or decorator

runHelper :: Monad m => [Value] -> Helper m -> BMX m Value
runHelper _ (BlockHelper _) = err (TypeError "helper" "block helper")
runHelper v (Helper h) = runFunctionT v [] h >>= either helpE return

runBlockHelper :: Monad m => [Value] -> BlockParams -> Template -> Template -> Helper m -> BMX m Page
runBlockHelper _ _ _ _ (Helper _) = err (TypeError "block helper" "helper")
runBlockHelper v bparams ifp elsep (BlockHelper h) = do
  fun <- runFunctionT v (toParams bparams) (h ifp elsep)
  either helpE return fun

-- Assumes @partial-block has been registered by caller.
runPartial :: (Applicative m, Monad m) => [Value] -> Partial m -> BMX m Page
runPartial v (Partial p) = partial
  where
    partial = runFunctionT v [] partialArg >>= either partE return
    partialArg = try customCtx <|> noCtx
    --
    customCtx = do
      (ContextV c) <- context
      liftBMX (withContext c p)
    --
    noCtx = liftBMX p

-- | Run a Decorator, then a continuation in the same environment
withDecorator :: Monad m => [Value] -> Decorator m -> BMX m Page -> BMX m Page
withDecorator _ (BlockDecorator _) _ = err (TypeError "decorator" "block decorator")
withDecorator v (Decorator d) k = runFunctionT v [] (d k) >>= either decoE return

-- | Run a block decorator, then a continuation
withBlockDecorator :: Monad m => [Value] -> Template -> Decorator m -> BMX m Page -> BMX m Page
withBlockDecorator _ _ (Decorator _) _ = err (TypeError "block decorator" "decorator")
withBlockDecorator v b (BlockDecorator d) k = runFunctionT v [] (d b k) >>= either decoE return


-- -----------------------------------------------------------------------------
-- Util

helpE :: Monad m => FunctionError -> BMX m a
helpE = err . HelperError

partE :: Monad m => FunctionError -> BMX m a
partE = err . PartialError

decoE :: Monad m => FunctionError -> BMX m a
decoE = err . DecoratorError

toParams :: BlockParams -> [Param]
toParams (BlockParams ps) = fmap (Param . renderLiteral) ps

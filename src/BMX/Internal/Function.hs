{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Internal.Function where

import           BMX.Data

import           P

type Helper m = HelperT (BMX m)
type Decorator m = DecoratorT (BMX m)
type Partial m = PartialT (BMX m)

helper :: Monad m => FunctionT (BMX m) Value -> Helper m
helper = Helper

blockHelper :: Monad m => (Template -> Template -> FunctionT (BMX m) Page) -> Helper m
blockHelper = BlockHelper

decorator :: Monad m => (BMX m Page -> FunctionT (BMX m) Page) -> Decorator m
decorator = Decorator

blockDecorator :: Monad m => (Template -> BMX m Page -> FunctionT (BMX m) Page) -> Decorator m
blockDecorator = BlockDecorator

partial :: Monad m => BMX m Page -> Partial m
partial = Partial

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

runBlockHelper :: Monad m => [Value] -> [Param] -> Template -> Template -> Helper m -> BMX m Page
runBlockHelper _ _ _ _ (Helper _) = err (TypeError "block helper" "helper")
runBlockHelper v bparams ifp elsep (BlockHelper h) = do
  fun <- runFunctionT v bparams (h ifp elsep)
  either helpE return fun

-- | Run a partial in the current environment, returning a @Page@.
-- Assumes @partial-block has been registered by caller.
runPartial :: (Applicative m, Monad m) => Partial m -> BMX m Page
runPartial (Partial p) = p

-- | Run a Decorator, then a continuation in the same environment
withDecorator :: Monad m => [Value] -> Decorator m -> BMX m Page -> BMX m Page
withDecorator _ (BlockDecorator _) _ = err (TypeError "decorator" "block decorator")
withDecorator v (Decorator d) k = runFunctionT v [] (d k) >>= either decoE return

-- | Run a block decorator, then a continuation
withBlockDecorator :: Monad m => [Value] -> Template -> Decorator m -> BMX m Page -> BMX m Page
withBlockDecorator _ _ (Decorator _) _ = err (TypeError "block decorator" "decorator")
withBlockDecorator v b (BlockDecorator d) k = runFunctionT v [] (d b k) >>= either decoE return

helpE :: Monad m => FunctionError -> BMX m a
helpE = err . HelperError

decoE :: Monad m => FunctionError -> BMX m a
decoE = err . DecoratorError

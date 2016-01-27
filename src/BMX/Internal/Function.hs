{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Internal.Function where

import           BMX.Data

import           P

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
runHelper v (Helper help) = case help of
  BlockHelperT _ -> err (TypeError "helper" "block helper")
  HelperT h -> runFunctionT v [] h >>= either helpE return

runBlockHelper :: Monad m => [Value] -> [Param] -> Template -> Template -> Helper m -> BMX m Page
runBlockHelper v bparams ifp elsep (Helper help) = case help of
  HelperT _ -> err (TypeError "block helper" "helper")
  BlockHelperT h -> do
    fun <- runFunctionT v bparams (h ifp elsep)
    either helpE return fun

-- | Run a partial in the current environment, returning a @Page@.
-- Assumes @partial-block has been registered by caller.
runPartial :: (Applicative m, Monad m) => Partial m -> BMX m Page
runPartial (Partial (PartialT p)) = p

-- | Run a Decorator, then a continuation in the same environment
withDecorator :: Monad m => [Value] -> Decorator m -> BMX m Page -> BMX m Page
withDecorator v (Decorator deco) k = case deco of
  BlockDecoratorT _ -> err (TypeError "decorator" "block decorator")
  DecoratorT d -> runFunctionT v [] (d k) >>= either decoE return

-- | Run a block decorator, then a continuation
withBlockDecorator :: Monad m => [Value] -> Template -> Decorator m -> BMX m Page -> BMX m Page
withBlockDecorator v b (Decorator deco) k = case deco of
  DecoratorT _ -> err (TypeError "block decorator" "decorator")
  BlockDecoratorT d -> runFunctionT v [] (d b k) >>= either decoE return

helpE :: Monad m => FunctionError -> BMX m a
helpE = err . HelperError

decoE :: Monad m => FunctionError -> BMX m a
decoE = err . DecoratorError

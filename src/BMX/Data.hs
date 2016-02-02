{- | Frontend types, and re-exports all data. Internal -}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data (
    BMXState (..)
  , BMXValue (..)
  , usingContext
  , usingPartials
  , usingHelpers
  , usingDecorators
  , packState
  , module X
  ) where

import           BMX.Data.AST as X
import           BMX.Data.Error as X
import           BMX.Data.Eval as X
import           BMX.Data.Format as X
import           BMX.Data.Function as X
import           BMX.Data.Page as X
import           BMX.Data.Token as X
import           BMX.Data.Value as X

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)

import           P

-- | BMXState holds the initial rendering environment, i.e. all bound helpers,
-- partials, decorators, and the current variable context.
--
-- The type parameter @m@ refers to the base monad for the registered
-- helpers, partials and decorators. It is commonly 'Identity' or
-- 'IO', though it should be possible to render a 'Template' on top of
-- any monad stack.
data BMXState m = BMXState
  { bmxContext :: [(Text, BMXValue)]
  , bmxPartials :: [(Text, Partial m)]
  , bmxHelpers :: [(Text, Helper m)]
  , bmxDecorators :: [(Text, Decorator m)]
  }

instance Monoid (BMXState m) where
  mempty = BMXState mempty mempty mempty mempty
  mappend a b = BMXState {
      bmxContext = bmxContext a <> bmxContext b
    , bmxPartials = bmxPartials a <> bmxPartials b
    , bmxHelpers = bmxHelpers a <> bmxHelpers b
    , bmxDecorators = bmxDecorators a <> bmxDecorators b
    }

data BMXValue
  = BMXString !Text
  | BMXNum !Integer
  | BMXBool !Bool
  | BMXContext ![(Text, BMXValue)]
  | BMXList ![BMXValue]
  | BMXNull

-- | Set the initial context in an 'BMXState'.
usingContext :: (Applicative m, Monad m) => BMXState m -> [(Text, BMXValue)] -> BMXState m
usingContext st c = st { bmxContext = c }

-- | Add a named collection of partials to the 'BMXState'.
usingPartials :: (Applicative m, Monad m) => BMXState m -> [(Text, Partial m)] -> BMXState m
usingPartials st ps = st { bmxPartials = ps <> bmxPartials st }

-- | Add a named collection of helpers to the 'BMXState'.
usingHelpers :: (Applicative m, Monad m) => BMXState m -> [(Text, Helper m)] -> BMXState m
usingHelpers st hs = st { bmxHelpers = hs <> bmxHelpers st }

-- | Add a named collection of decorators to the 'BMXState'.
usingDecorators :: (Applicative m, Monad m) => BMXState m -> [(Text, Decorator m)] -> BMXState m
usingDecorators st ds = st { bmxDecorators = ds <> bmxDecorators st }

-- | Pack the association lists from 'BMXState' into the maps of 'EvalState',
-- throwing errors whenever shadowing is encountered.
packState :: (Applicative m, Monad m) => BMXState m -> Either BMXError (EvalState m)
packState bst = do
  ctx <- boxContext (bmxContext bst)
  partials <- mapUnique Shadowing (bmxPartials bst)
  helpers <- mapUnique Shadowing (bmxHelpers bst)
  decorators <- mapUnique Shadowing (bmxDecorators bst)
  let dta = mempty
  return EvalState {
      evalContext = [ctx]
    , evalData = dta
    , evalHelpers = helpers
    , evalPartials = partials
    , evalDecorators = decorators
    }

mapUnique :: (Text -> EvalError) -> [(Text, a)] -> Either BMXError (Map Text a)
mapUnique e = foldM foldFun M.empty
  where foldFun m (k, v)  = if M.member k m
          then Left (BMXEvalError (e k))
          else Right (M.insert k v m)

boxContext :: [(Text, BMXValue)] -> Either BMXError Context
boxContext c = do
  ctx <- mapUnique Shadowing c
  ctx' <- mapM rebox ctx
  return (Context ctx')

rebox :: BMXValue -> Either BMXError Value
rebox v = case v of
  BMXString t -> pure (StringV t)
  BMXNum i -> pure (IntV i)
  BMXBool b -> pure (BoolV b)
  BMXNull -> pure NullV
  BMXList ls -> ListV <$> mapM rebox ls
  BMXContext c -> ContextV <$> boxContext c

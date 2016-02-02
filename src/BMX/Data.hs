{- | Frontend types, and re-exports all data. Internal -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data (
    BMXState (..)
  , usingContext
  , usingPartials
  , usingHelpers
  , usingDecorators
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
  { bmxContext :: Context
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

-- | Set the initial context in an 'BMXState'.
usingContext :: (Applicative m, Monad m) => BMXState m -> Context -> BMXState m
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

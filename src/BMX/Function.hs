{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Function (
  -- * Helpers
    Helper
  , helper
  , blockHelper
  -- * Decorators
  , Decorator
  , decorator
  , blockDecorator
  -- * Partials
  , Partial
  , partial
  -- * Argument parsers
  , FunctionT
  , value
  , string
  , number
  , boolean
  , nullv
  , undef
  , context
  , list
  -- * BMX
  , BMX
  , eval
  , liftBMX
  -- ** Changing stuff
  , withVariable
  , withContext
  , withData
  , withPartial
  -- ** Querying stuff
  , lookupValue
  , lookupData
  , lookupHelper
  , lookupDecorator
  -- ** Consuming functions
  , runHelper
  , runBlockHelper
  , runPartial
  , withDecorator
  , withBlockDecorator
  -- ** Utility functions
  , contextToList
  ) where

import           BMX.Data
import           BMX.Eval (eval)
import           BMX.Internal.Function

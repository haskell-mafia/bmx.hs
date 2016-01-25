{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.Format (
    Format (..)
  , renderFormat
  ) where

import           Data.Data (Data, Typeable)
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.Generics (Generic)

import           P

-- | Formatting control
data Format
  = Strip     -- ^ Strip all adjacent whitespace in some direction
  | Verbatim  -- ^ Leave adjacent nodes intact, don't strip
  deriving (Show, Eq, Generic, Data, Typeable)

renderFormat :: Format -> Text
renderFormat = \case
  Strip    -> "~"
  Verbatim -> T.empty

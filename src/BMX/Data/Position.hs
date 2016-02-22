{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module BMX.Data.Position (
    SrcInfo (..)
  , Position (..)
  , Positioned (..)
  , (<@@)
  , (@@>)
  ) where

import           P

-- | A single point in the source file.
data Position = Position {
    posLine :: !Int
  , posColumn :: !Int
  } deriving (Eq, Ord, Show)

-- | A range in the source file.
data SrcInfo = SrcInfo {
    leadingPosition :: !Position
  , trailingPosition :: !Position
  } deriving (Eq, Ord, Show)

combine :: SrcInfo -> SrcInfo -> SrcInfo
combine a b = SrcInfo {
    leadingPosition = leadingPosition a
  , trailingPosition = trailingPosition b
  }

-- | A value and character range pair
data Positioned a = !a :@ SrcInfo
  deriving (Eq, Ord, Show)

instance Functor Positioned where
  fmap f (x :@ info) = f x :@ info
  x <$ (_ :@ info) = x :@ info

-- Absorb the item to the right
(<@@) :: Positioned a -> Positioned b -> Positioned a
(x :@ i) <@@ (_ :@ j) = x :@ (i `combine` j)

-- Absorb the item to the left
(@@>) :: Positioned a -> Positioned b -> Positioned b
(_ :@ i) @@> (y :@ j) = y :@ (i `combine` j)

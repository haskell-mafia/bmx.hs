{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module BMX.Data.Position (
    SrcInfo (..)
  , Position (..)
  , Positioned (..)
  , depo
  , (<@@)
  , (@@>)
  , between
  , renderPosition
  , renderSrcInfo
  ) where

import           Data.Data (Data, Typeable)
import           Data.Text (Text)
import qualified Data.Text as T

import           P

-- | A single point in the source file.
data Position = Position {
    posLine :: !Int
  , posColumn :: !Int
  } deriving (Data, Eq, Ord, Show, Typeable)

emptyPosition :: Position
emptyPosition = Position 0 0

renderPosition :: Position -> Text
renderPosition pos = "Line " <> tshow (posLine pos) <> ", Col " <> tshow (posColumn pos)

-- | A range in the source file.
data SrcInfo = SrcInfo {
    leadingPosition :: !Position
  , trailingPosition :: !Position
  } deriving (Data, Eq, Ord, Show, Typeable)

instance Monoid SrcInfo where
  mempty = SrcInfo emptyPosition emptyPosition
  mappend a b = SrcInfo (leadingPosition a) (trailingPosition b)

renderSrcInfo :: SrcInfo -> Text
renderSrcInfo srci = renderPosition (leadingPosition srci)
  <> " -- " <> renderPosition (trailingPosition srci)

-- | A value and character range pair
data Positioned a = !a :@ !SrcInfo
  deriving (Data, Eq, Ord, Show, Typeable)

instance Monoid a => Monoid (Positioned a) where
  mempty = mempty :@ mempty
  mappend (a :@ la) (b :@ lb) = (a <> b) :@ (la <> lb)

instance Functor Positioned where
  fmap f (x :@ info) = f x :@ info
  x <$ (_ :@ info) = x :@ info

instance Applicative Positioned where
  (a :@ la) <*> (b :@ lb) = (a b) :@ (la <> lb)
  pure a = a :@ mempty

-- | Strip position information
depo :: Positioned a -> a
depo (a :@ _) = a

-- | Absorb the item to the right
(<@@) :: Positioned a -> Positioned b -> Positioned a
(x :@ i) <@@ (_ :@ j) = x :@ (i <> j)

-- | Absorb the item to the left
(@@>) :: Positioned a -> Positioned b -> Positioned b
(_ :@ i) @@> (y :@ j) = y :@ (i <> j)

between :: Positioned a -> Positioned b -> SrcInfo
between (_ :@ la) (_ :@ lb) = la <> lb

tshow :: Show a => a -> Text
tshow = T.pack . show

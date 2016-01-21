{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module BMX.Data.Page (
    Page (..)
  , page
  , content
  , renderPage
  , escapePage
  ) where

import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import qualified Text.Blaze.Html as B
import qualified Text.Blaze.Html.Renderer.Text as B

import           BMX.Data.Format

import           P

-- | The type of rendered templates.
data Page
  = Formatter !Format !Format !Text !Text !Text
  | Formattee !Text
  deriving (Show, Eq)

instance Monoid Page where
  mempty = Formattee T.empty
  mappend (Formattee c1) (Formattee c2) = Formattee (c1 <> c2)
  mappend (Formattee c) (Formatter lf rf body lt rt) =
    let lhs = formatEnd lf c
        lf' = newFormat lf lhs
        lt' = lhs <> lt
    in Formatter lf' rf body lt' rt
  mappend (Formatter lf rf body lt rt) (Formattee c) =
    let rhs = formatStart rf c
        rt' = rt <> rhs
        rf' = newFormat rf rhs
    in Formatter lf rf' body lt rt'
  mappend (Formatter lf1 rf1 body1 lt1 rt1) (Formatter lf2 rf2 body2 lt2 rt2) =
    let rt1' = formatEnd lf2 rt1
        lt2' = formatStart rf1 lt2
    in Formatter lf1 rf2 (body1 <> rt1' <> lt2' <> body2) lt1 rt2

{-

FIX: most nodes need to have their lines collapsed (let's call it Inlining)

"This expands the default behavior of stripping lines that are "standalone" helpers
 (only a block helper, comment, or partial and whitespace)."

In other words, the following statement types need a special kind of 'Verbatim'
that removes all whitespace up to and including the first newline:

- Partial (though there's also a gggreat indenting feature!!)
- PartialBlock
- Block
- Inverse
- InverseChain
- InverseBlock
- RawBlock (top and tail, different from the others!!! Gets its own constructor!!!)
- CommentStmt
- DecoratorBlock

FIX: Partials need to know about their indentation level. This can probably be done
     with another variant of PageFormat and some more cases in mappend, plus some Text
     indentation magic (T.unlines $ fmap (mappend whitespace) (T.lines renderedPartial))

-}

page :: Format -> Format -> Text -> Page
page l r body = Formatter l r body T.empty T.empty

content :: Text -> Page
content = Formattee

newFormat :: Format -> Text -> Format
newFormat Verbatim _ = Verbatim
newFormat Strip t = if T.all isSpace t then Strip else Verbatim

renderPage :: Page -> Text
renderPage (Formatter _ _ body lt rt) = lt <> body <> rt
renderPage (Formattee t) = t

-- | Apply formatting to the end of a page
formatEnd :: Format -> Text -> Text
formatEnd Verbatim t = t
formatEnd Strip t = stripLeft t
  where stripLeft = T.dropWhileEnd isSpace

-- | Apply formatting to the start of a page
formatStart :: Format -> Text -> Text
formatStart Verbatim t = t
formatStart Strip t = stripRight t
  where stripRight = T.dropWhile isSpace

escapePage :: Page -> Page
escapePage p@(Formattee _) = p
escapePage (Formatter lf rf body lt rt) = Formatter lf rf (escapeText body) lt rt
  where escapeText = T.toStrict . B.renderHtml . B.toHtml

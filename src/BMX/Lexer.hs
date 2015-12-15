{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
module BMX.Lexer (tokenise, LexError (..)) where

import           Data.Attoparsec.Text as A
import           Data.Attoparsec.Combinator (lookAhead)
-- import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T

import           BMX.Data (Token (..), Format(..))

import           P

newtype LexError = LexError { renderLexError :: Text }
  deriving (Eq)

tokenise :: Text -> Either LexError [Token]
tokenise t = case parse tokens t of
  Partial c  -> render (c T.empty)
  a          -> render a
  where render (Done "" ts) = Right ts
        render (Done r _)   = Left (LexError $ "Invalid input: lexer failed at " <> T.take 10 r)
        render (Fail _ _ s) = Left (LexError (T.pack s))
        render (Partial _)  = Left (LexError "Incomplete parse")

tokens :: Parser [Token]
tokens = mconcat <$> many token


-- -----------------------------------------------------------------------------
-- Character classes
--

notNull :: Parser Char
notNull = satisfy (/= '\0')

{-
idLookAhead :: Parser Char
idLookAhead = satisfy predi
  where predi c = or [isSpace c, inClass idClass c]
        idClass = ['=', '~', '}', '/', '.', ')', '|']

literalLookAhead :: Parser Char
literalLookAhead = satisfy predi
  where predi c = or [isSpace c, inClass litClass c]
        litClass = ['~', '}', ')']

validIdChar :: Parser Char
validIdChar = satisfy predi
  where predi c = and [not (isSpace c), notInClass idNegClass c]
        -- | This is a horrible mismash of ranges translated from the official lexer
        idNegClass =    ['!', '"', '#']
                     <> ['%' .. ',']
                     <> ['.', '/', ';', '<', '=', '>', '@', '[', '\\', ']', '^', '`']
                     <> ['{', '|', '}', '~']
-}

-- -----------------------------------------------------------------------------
-- Top-level
--

token :: Parser [Token]
token = mu <|> content

-- | Raw Web Content
content :: Parser [Token]
content = do
  body <- go <|> plain
  guard (not (T.null body))
  pure [Content body]
  where
    plain = T.pack <$> many1 notNull
    --
    go :: Parser Text
    go = do
      str <- manyTill' notNull (lookAhead open)
      let c = T.pack str
      -- Handlebars blocks can be escaped with a single \
      if | T.takeEnd 2 c == "\\\\" -> pure (T.dropEnd 1 c)
         | T.takeEnd 1 c == "\\"   -> do
             o <- open
             g <- go
             pure (c <> o <> g)
         | otherwise               -> pure c

-- | Mustachioed blocks
mu :: Parser [Token]
mu = do
  _         <- open
  lf        <- strip
  (o, body) <- blockComment lf <|> muExpr lf
  pure (o : body)

-- | Mustachioed expressions
muExpr :: Format -> Parser (Token, [Token])
muExpr f = do
  o      <- openPs
  -- FIX should a space be mandatory here?
  expr   <- manyTill' exprPs (lookAhead (strip *> close))
  rstrip <- strip
  _      <- close
  pure (o, expr <> [Close rstrip])
  where
    openPs = openPartial f
         <|> openPartialBlock f
         <|> openBlock f
         <|> openEndBlock f
         <|> openUnescaped f
         <|> openOrdinary f
         <|> openInverse f
         <|> openInverseChain f
    --
    exprPs = openSExp <|> closeSExp

-- | Top-level comment (block containing only a comment)
-- e.g. "{{!-- this is a top-level comment --}}"
blockComment :: Format -> Parser (Token, [Token])
blockComment f = do
  o      <- startComment
  com    <- manyTill' anyChar (lookAhead endCommentBlock)
  _      <- endComment
  rstrip <- strip
  _      <- close
  pure (o f, [Comment (T.pack com)] <> [CloseCommentBlock rstrip])
  where startComment = string "!--" *> pure OpenCommentBlock
        endComment = string "--"
        endCommentBlock = endComment *> strip *> close


-- -----------------------------------------------------------------------------
-- Handlebars expression prologues
--

openPartial :: Format -> Parser Token
openPartial f = char '>' *> pure (OpenPartial f)

openPartialBlock :: Format -> Parser Token
openPartialBlock f = string "#>" *> pure (OpenPartialBlock f)

openBlock :: Format -> Parser Token
openBlock f = char '#' *> option '*' (char '*') *> pure (OpenBlock f)

openEndBlock :: Format -> Parser Token
openEndBlock f = char '/' *> pure (OpenEndBlock f)

openUnescaped :: Format -> Parser Token
openUnescaped f = char '{' *> pure (OpenUnescaped f)

openOrdinary :: Format -> Parser Token
openOrdinary f = char '&' *> pure (Open f)

openInverse :: Format -> Parser Token
openInverse f = char '^' *> pure (OpenInverse f)

openInverseChain :: Format -> Parser Token
openInverseChain f = skipSpace *> string "else" *> pure (OpenInverseChain f)


-- -----------------------------------------------------------------------------
-- Handlebars expressions
--

openSExp :: Parser Token
openSExp = const OpenSExp <$> char '('

closeSExp :: Parser Token
closeSExp = const CloseSExp <$> char ')'

strip :: Parser Format
strip = option Verbatim (string "~" *> pure Strip)

open :: Parser Text
open = string "{{"

close :: Parser Text
close = string "}}"

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
module BMX.Lexer (
    LexError (..)
  , tokenise
  , tokens
  , content
  , mu
  , validIdChar
  ) where

import           Data.Attoparsec.Text as A
import           Data.Attoparsec.Combinator (lookAhead)
import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T

import           BMX.Data

import           P hiding (null)


newtype LexError = LexError { renderLexError :: Text }
  deriving (Eq)


tokenise :: Text -> Either LexError [Token]
tokenise t = bimap (LexError . T.pack) id (A.parseOnly tokens t)

tokens :: Parser [Token]
tokens = mconcat <$> many token <* endOfInput


-- -----------------------------------------------------------------------------
-- Character classes
--

notNull :: Parser Char
notNull = satisfy (/= '\0')

-- | When one these characters follow, '.' and ".." get treated as IDs
idLookAhead :: Parser Char
idLookAhead = satisfy predi
  where predi c = or [isSpace c, inClass idClass c]
        idClass = ['=', '~', '}', '/', '.', ')', '|']

literalLookAhead :: Parser Char
literalLookAhead = satisfy predi
  where predi c = or [isSpace c, inClass litClass c]
        litClass = ['~', '}', ')']

validIdChar :: Char -> Bool
validIdChar = predi
  where predi c = and [not (isSpace c), notInClass idNegClass c]
        -- | This is a horrible mismash of ranges translated from the official lexer
        idNegClass =    ['!', '"', '#', '\0']
                     <> ['%' .. ',']
                     <> ['.', '/', ';', '<', '=', '>', '@', '[', '\\', ']', '^', '`']
                     <> ['{', '|', '}', '~']

-- | The Close tag for a raw block uses its own lookahead class
rawCloseLookAhead :: Parser Char
rawCloseLookAhead = satisfy predi
  where predi c = or [isSpace c, inClass rawClass c]
        rawClass = ['=', '}', '/', '.']


-- -----------------------------------------------------------------------------
-- Top-level
--

token :: Parser [Token]
token = mu <|> content

-- | Raw Web Content
content :: Parser [Token]
content = do
  body <- manyTillUnescaped notNull open <|> plain
  guard (not (T.null body))
  pure [Content body]
  where
    plain = T.pack <$> many1 notNull

-- | Mustachioed blocks
mu :: Parser [Token]
mu = do
  _    <- open
  lf   <- strip
  body <- blockComment lf <|> shortComment lf <|> unescapedMu lf <|> muExpr lf <|> rawBlock lf
  pure body

-- | Mustachioed expressions
muExpr :: Format -> Parser [Token]
muExpr f = do
  o      <- openPs f
  expr   <- manyTill' exprPs (lookAhead (strip *> close))
  rstrip <- strip
  _      <- close
  pure (o : expr <> [Close rstrip])

-- | Top-level comment (block containing only a comment)
-- e.g. "{{!-- this is a top-level comment --}}"
-- Doesn't allow nested comments. "{{!-- {{!-- --}} --}}" will fail.
-- See https://github.com/wycats/handlebars.js/blob/master/src/handlebars.l#L68
blockComment :: Format -> Parser [Token]
blockComment f = do
  o      <- startComment
  com    <- manyTill' notNull (lookAhead endCommentBlock)
  _      <- endComment
  rstrip <- strip
  _      <- close
  pure (o f : [Comment (T.pack com), CloseCommentBlock rstrip])
  where startComment = string "!--" *> pure OpenCommentBlock
        endComment = string "--"
        endCommentBlock = endComment *> strip *> close

-- | Top-level comment in the short format. These can't contain
-- anything mustache would parse.
-- See https://github.com/wycats/handlebars.js/blob/master/src/handlebars.l#L97
shortComment :: Format -> Parser [Token]
shortComment f = do
  _   <- char '!'
  com <- T.pack <$> manyTill' notNull close
  -- Spec has no rstrip here
  pure [OpenComment f, Comment com, Close Verbatim]

-- | Values produced by these blocks are not HTML-escaped
unescapedMu :: Format -> Parser [Token]
unescapedMu lf = do
  o    <- openUnescaped lf
  body <- manyTill' exprPs (lookAhead endEscaped)
  rf   <- endEscaped
  pure (o : body <> [rf])
  where endEscaped = CloseUnescaped <$> (string "}" *> strip <* close)

-- | A raw block: All content in between {{{{helper foo bar}}}} and {{{{/helper}}}} is
-- handed directly to helper without any mustache-processing.
-- Nested raw blocks are supported (ignored), but need to be balanced.
-- e.g. {{{{a}}}} {{{{this does not work}}}} {{{{/a}}}}
rawBlock :: Format -> Parser [Token]
rawBlock Strip = fail "raw blocks can't perform formatting"
rawBlock Verbatim = do
  _    <- open
  body <- manyTill' exprPs (close *> close)
  raw  <- T.concat <$> many (nestedRaw <|> content')
  c    <- closeRawBlock
  pure (OpenRawBlock : body <> [CloseRawBlock, RawContent raw, c])
  where
    openRaw = string "{{{{"
    closeRaw = string "}}}}"
    --
    content' = do
      a <- T.pack <$> manyTill' notNull (lookAhead (openRaw <|> closeRaw))
      guard (not (T.null a))
      pure a
    -- Nested raw blocks are parsed as Content, but nested blocks ntb balanced
    nestedRaw :: Parser Text
    nestedRaw = do
      o <- openRaw
      peekChar' >>= guard . (/= '/')
      body <- manyTill' notNull (lookAhead closeRaw)
      c <- closeRaw
      bs <- many (nestedRaw <|> content')
      i <- closeRawBlock
      pure (o <> T.pack body <> c <> T.concat bs <> renderToken i)
    --
    closeRawBlock = do
      _ <- openRaw
      _ <- char '/'
      i <- takeWhile validIdChar
      _ <- lookAhead rawCloseLookAhead
      _ <- closeRaw
      pure (CloseRaw i)

openPs :: Format -> Parser Token
openPs f = openPartial f
       <|> openPartialBlock f
       <|> openBlock f
       <|> openEndBlock f
       <|> openUnescaped f
       <|> openUnescaped' f
       <|> openInverse f
       <|> openInverseChain f
       <|> openOrdinary f

exprPs :: Parser Token
exprPs = skipSpace *> eps <* skipSpace
  where eps = numberP
          <|> stringP
          <|> openSExp
          <|> closeSExp
          <|> equals
          <|> dataSigil
          <|> undef
          <|> null
          <|> openBlockParams
          <|> closeBlockParams
          <|> idP
          <|> sep


-- -----------------------------------------------------------------------------
-- Handlebars expression prologues
--

openPartial :: Format -> Parser Token
openPartial f = char '>' *> pure (OpenPartial f)

openPartialBlock :: Format -> Parser Token
openPartialBlock f = string "#>" *> pure (OpenPartialBlock f)

-- | {{# - block syntax
--  {{#* - decorator block
openBlock :: Format -> Parser Token
openBlock f = do
  _ <- char '#'
  option (OpenBlock f) (char '*' *> pure (OpenDecoratorBlock f))

-- | End of a block's scope.
-- e.g. {{/
openEndBlock :: Format -> Parser Token
openEndBlock f = char '/' *> pure (OpenEndBlock f)

-- | A value that should not be HTML-escaped
-- e.g. {{{body}}}
openUnescaped :: Format -> Parser Token
openUnescaped f = char '{' *> pure (OpenUnescaped f)

-- | Alternate, undocumented format for unescaped output
-- e.g. {{&body}}
openUnescaped' :: Format -> Parser Token
openUnescaped' f = char '&' *> pure (OpenUnescaped f)

-- | {{ - ordinary expression
--  {{* - decorator
openOrdinary :: Format -> Parser Token
openOrdinary f = option (Open f) (char '*' *> pure (OpenDecorator f))

openInverse :: Format -> Parser Token
openInverse f = char '^' *> pure (OpenInverse f)

openInverseChain :: Format -> Parser Token
openInverseChain f = skipSpace *> string "else" *> pure (OpenInverseChain f)


-- -----------------------------------------------------------------------------
-- Handlebars expressions
--

-- | IDs are either ".", "..", a sequence of chars satisfying validIdChar, or
-- 'segment literal notation' e.g. [10].
-- Segment literals are defined as [(\\]|[^\]])*], i.e. [.*] with escaping
idP :: Parser Token
idP = segLit <|> dotdot <|> dot <|> idP'
  where
    dotdot = ID <$> string ".."
    --
    dot = do
      c <- string "."
      _ <- lookAhead idLookAhead
      pure (ID c)
    --
    idP' = do
      i <- takeWhile1 validIdChar
      _ <- lookAhead idLookAhead
      guard (not (T.null i))
      pure (ID i)
    --
    segLit = do
      _   <- char '['
      lit <- manyTillUnescaped notNull (string "]")
      _   <- char ']'
      pure (SegmentID lit)

numberP :: Parser Token
numberP = do
  int <- signed decimal
  _   <- option 0 suffix -- discard anything past the decimal point
  _   <- lookAhead literalLookAhead
  pure (Number int)
  where suffix = char '.' *> signed decimal :: Parser Integer

stringP :: Parser Token
stringP = do
  _   <- char '"'
  str <- manyTillUnescaped notNull (string "\"")
  _   <- char '"'
  pure (String (T.replace "\\\"" "\"" str))

sep :: Parser Token
sep = Sep <$> (char '.' <|> char '/')

equals :: Parser Token
equals = string "=" *> pure Equals

dataSigil :: Parser Token
dataSigil = string "@" *> pure Data

undef :: Parser Token
undef = string "undefined" *> pure Undefined

null :: Parser Token
null = string "null" *> pure Null

openBlockParams :: Parser Token
openBlockParams = string "as" *> many1 space *> string "|" *> pure OpenBlockParams

closeBlockParams :: Parser Token
closeBlockParams = string "|" *> pure CloseBlockParams

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


-- -----------------------------------------------------------------------------
-- Util
--

manyTillUnescaped :: Parser Char -> Parser Text -> Parser Text
manyTillUnescaped a special = do
  str <- manyTill' a (lookAhead special)
  let c = T.pack str
  if | T.takeEnd 2 c == "\\\\" -> pure (T.dropEnd 1 c)
     | T.takeEnd 1 c == "\\"   -> do
         o <- special
         g <- manyTillUnescaped a special
         pure (c <> o <> g)
     | otherwise               -> pure c

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
module BMX.Lexer (
    LexError (..)
  , tokenise
  , validIdChar
  ) where

import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Parsec hiding ((<|>), string, tokens, token)
import qualified Text.Parsec as Parsec
import           Text.Parsec.Text
import           Text.Read (read)

import           BMX.Data

import           P hiding (many, null)


tokenise :: Text -> Either LexError [Token]
tokenise t = bimap (LexError . T.pack . show) id (Parsec.parse tokens "handlebars" t)

tokens :: Parser [Token]
tokens = mconcat <$> many token <* eof


-- -----------------------------------------------------------------------------
-- Character classes
--

notNull :: Parser Char
notNull = satisfy (/= '\0')

-- | When one these characters follow, '.' and ".." get treated as IDs
idLookAhead :: Parser Char
idLookAhead = try $ satisfy predi
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


-- -----------------------------------------------------------------------------
-- Top-level
--

token :: Parser [Token]
token = mu <|> contentP

-- | Raw Web Content
contentP :: Parser [Token]
contentP = do
  body <- try (manyTillUnescaped notNull open) <|> plain
  guard (not (T.null body))
  pure [Content body]
  where
    plain = T.pack <$> many1 notNull

-- | Mustachioed blocks
mu :: Parser [Token]
mu = do
  _    <- try open
  lf   <- strip
  body <- blockComment lf <|> shortComment lf <|> rawBlock lf <|> unescapedMu lf <|> muExpr lf
  pure body

-- | Mustachioed expressions
muExpr :: Format -> Parser [Token]
muExpr f = do
  o      <- try $ openPs f
  expr   <- manyTill exprPs (lookAhead (try (strip *> close)))
  rstrip <- strip
  _      <- close
  pure (o : expr <> [Close rstrip])

-- | Top-level comment (block containing only a comment)
-- e.g. "{{!-- this is a top-level comment --}}"
-- Doesn't allow nested comments. "{{!-- {{!-- --}} --}}" will fail.
-- See https://github.com/wycats/handlebars.js/blob/master/src/handlebars.l#L68
blockComment :: Format -> Parser [Token]
blockComment f = do
  o      <- try startComment
  com    <- manyTill notNull (lookAhead (try endCommentBlock))
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
  _   <- try $ char '!'
  com <- T.pack <$> manyTill notNull close
  -- Spec has no rstrip here
  pure [OpenComment f, Comment com, Close Verbatim]

-- | Values produced by these blocks are not HTML-escaped
unescapedMu :: Format -> Parser [Token]
unescapedMu lf = do
  o    <- try $ openUnescaped lf
  body <- manyTill exprPs (lookAhead (try endEscaped))
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
  _    <- try open
  body <- manyTill exprPs (close *> close)
  raw  <- T.concat <$> many (try (nestedRaw <|> content'))
  c    <- closeRawBlock
  pure (OpenRawBlock : body <> [CloseRawBlock, RawContent raw, c])
  where
    openRaw = string "{{{{"
    closeRaw = string "}}}}"
    --
    content' = do
      a <- try $ T.pack <$> manyTill notNull (lookAhead (try (openRaw <|> closeRaw)))
      guard (not (T.null a))
      pure a
    -- Nested raw blocks are parsed as Content, but nested blocks ntb balanced
    nestedRaw :: Parser Text
    nestedRaw = do
      o <- try (openRaw <* notFollowedBy (char '/'))
      body <- manyTill notNull (lookAhead (try closeRaw))
      c <- closeRaw
      bs <- many (nestedRaw <|> content')
      (CloseRaw i) <- closeRawBlock
      pure (o <> T.pack body <> c <> T.concat bs <> "{{{{/" <> i <> "}}}}")
    --
    closeRawBlock = do
      _ <- try $ openRaw *> char '/'
      i <- manyTill notNull (lookAhead (try (skipSpace *> closeRaw)))
      _ <- skipSpace *> closeRaw
      pure (CloseRaw (T.pack i))

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
       <?> "mustache opener"

exprPs :: Parser Token
exprPs = skipSpace *> eps <* skipSpace
  where eps = numberP
          <|> stringP
          <|> boolP
          <|> openSExp
          <|> closeSExp
          <|> equals
          <|> dataSigil
          <|> undef
          <|> null
          <|> openBlockParams
          <|> closeBlockParams
          <|> idP
          <?> "literal"


-- -----------------------------------------------------------------------------
-- Handlebars expression prologues
--

openPartial :: Format -> Parser Token
openPartial f = try $ char '>' *> pure (OpenPartial f)

openPartialBlock :: Format -> Parser Token
openPartialBlock f = try $ string "#>" *> pure (OpenPartialBlock f)

-- | {{# - block syntax
--  {{#* - decorator block
openBlock :: Format -> Parser Token
openBlock f = do
  _ <- try $ char '#'
  option (OpenBlock f) (try $ char '*' *> pure (OpenDecoratorBlock f))

-- | End of a block's scope.
-- e.g. {{/
openEndBlock :: Format -> Parser Token
openEndBlock f = try $ char '/' *> pure (OpenEndBlock f)

-- | A value that should not be HTML-escaped
-- e.g. {{{body}}}
openUnescaped :: Format -> Parser Token
openUnescaped f = try $ char '{' *> pure (OpenUnescaped f)

-- | Alternate, undocumented format for unescaped output
-- e.g. {{&body}}
openUnescaped' :: Format -> Parser Token
openUnescaped' f = try $ char '&' *> pure (OpenUnescaped f)

-- | {{ - ordinary expression
--  {{* - decorator
openOrdinary :: Format -> Parser Token
openOrdinary f = option (Open f) (try $ char '*' *> pure (OpenDecorator f))

openInverse :: Format -> Parser Token
openInverse f = try $ char '^' *> pure (OpenInverse f)

openInverseChain :: Format -> Parser Token
openInverseChain f = try $ (skipSpace *> string "else" <* skipSpace) *> pure (OpenInverseChain f)


-- -----------------------------------------------------------------------------
-- Handlebars expressions
--

-- | IDs are either ".", "..", a sequence of chars satisfying validIdChar, or
-- 'segment literal notation' e.g. [10].
-- Segment literals are defined as [(\\]|[^\]])*], i.e. [.*] with escaping
idP :: Parser Token
idP = segLit <|> dotdot <|> try dot <|> sep <|> idP'
  where
    idTrail = lookAhead (try idLookAhead)
    --
    dotdot = ID <$> try (string "..") <* idTrail
    --
    dot = do
      c <- try (char '.')
      _ <- try idTrail
      pure (ID (T.singleton c))
    --
    idP' = do
      i <- try $ takeWhile1 validIdChar
      _ <- try idTrail
      guard (not (T.null i))
      pure (ID i)
    --
    segLit = do
      _   <- try $ char '['
      lit <- manyTillUnescaped notNull (string "]")
      _   <- char ']'
      _   <- try idTrail
      pure (SegmentID lit)

numberP :: Parser Token
numberP = do
  neg <- option id (try $ char '-' *> pure negate)
  int <- try (read <$> many1 digit)
  _   <- option [] suffix -- discard any fractional component
  _   <- lookAhead (try literalLookAhead)
  pure (Number (neg int))
  where suffix = try $ char '.' *> many1 digit

stringP :: Parser Token
stringP = doubleP <|> singleP
  where
    doubleP = do
      _   <- try $ char '"'
      str <- manyTillUnescaped notNull (string "\"")
      _   <- char '"'
      pure (String (T.replace "\\\"" "\"" str))
    singleP = do
      _   <- try $ char '\''
      str <- manyTillUnescaped notNull (string "'")
      _   <- char '\''
      pure (String (T.replace "\\'" "'" str))

boolP :: Parser Token
boolP = true <|> false
  where
    true = try $ string "true" *> pure (Boolean True)
    false = try $ string "false" *> pure (Boolean False)

sep :: Parser Token
sep = Sep <$> (try (char '.') <|> try (char '/'))

equals :: Parser Token
equals = try $ string "=" *> pure Equals

dataSigil :: Parser Token
dataSigil = try $ string "@" *> pure Data

undef :: Parser Token
undef = try $ string "undefined" *> pure Undefined

null :: Parser Token
null = try $ string "null" *> pure Null

openBlockParams :: Parser Token
openBlockParams = try $ string "as" *> many1 space *> string "|" *> pure OpenBlockParams

closeBlockParams :: Parser Token
closeBlockParams = try $ string "|" *> pure CloseBlockParams

openSExp :: Parser Token
openSExp = try $ const OpenSExp <$> char '('

closeSExp :: Parser Token
closeSExp = try $ const CloseSExp <$> char ')'

strip :: Parser Format
strip = option Verbatim (try $ string "~" *> pure Strip)

open :: Parser Text
open = try $ string "{{"

close :: Parser Text
close = try $ string "}}"


-- -----------------------------------------------------------------------------
-- Util
--

manyTillUnescaped :: Parser Char -> Parser Text -> Parser Text
manyTillUnescaped a special = do
  str <- manyTill a (lookAhead (try special))
  let c = T.pack str
  if | T.takeEnd 2 c == "\\\\" -> pure (T.dropEnd 1 c)
     | T.takeEnd 1 c == "\\"   -> do
         o <- option T.empty special
         g <- manyTillUnescaped a special
         pure (c <> o <> g)
     | otherwise               -> pure c

skipSpace :: Parser ()
skipSpace = skipMany (try space)

-- Slow version of Attoparsec's
inClass :: [Char] -> Char -> Bool
inClass = flip elem

notInClass :: [Char] -> Char -> Bool
notInClass s = not . inClass s

takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = T.pack <$> many1 (try (satisfy p))

string :: [Char] -> Parser Text
string = fmap T.pack . Parsec.string

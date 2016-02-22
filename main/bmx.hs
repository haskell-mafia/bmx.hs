{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           BuildInfo_ambiata_bmx

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory (doesFileExist)
import           System.Environment (getProgName)
import           System.IO

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither)
import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Data.Aeson (asTextWith, asWith)
import           X.Options.Applicative

import           BMX
import           BMX.Lexer (tokenise)

import           P

-- done
-- -v --version                      - dump version from BuildInfo
-- -c --context file.json            - read context from json file
-- --verbose                         - dump the lexer tokens and parser tokens

-- todo
-- -t --template file.hbs            - read template from file (default stdin)
-- -p --partials filea.hbs fileb.hbs - list of partials. just drop the extension for their names
-- -d --debug                        - turn on debugging helpers

data BMXCommand =
    BMXVersion
  | BMXEval Verbosity (Maybe FilePath) -- (Maybe FilePath) [FilePath]

data Verbosity =
    Normal
  | Spam

data CLIError =
    TemplateError !BMXError
  | ContextError !Text
  -- | PartialError !BMXError

renderCLIError :: CLIError -> Text
renderCLIError = \case
  TemplateError e -> "[template] " <> renderBMXError e
  ContextError e -> "[context] " <> e
--  PartialError e -> "[partial] " <> renderBMXError e

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  dispatch bmxP >>= \case
    BMXVersion -> do
      prog <- getProgName
      putStrLn (prog <> ": " <> buildInfoVersion)

    BMXEval v c -> orDie renderCLIError (bmxEval v c)

bmxEval :: Verbosity -> (Maybe FilePath) -> EitherT CLIError IO ()
bmxEval v f = do
  -- Grab context from file, if it exists
  ctx <- maybe (return mempty) loadCtx f

  -- Grab input from stdin
  inp <- liftIO T.getContents

  -- dispatch to verbose or concise
  case v of
    Normal -> bmxConcise inp ctx
    Spam -> bmxSpam inp ctx

loadCtx :: FilePath -> EitherT CLIError IO [(Text, BMXValue)]
loadCtx c = do
  ctx <- liftIO $ do
    b <- doesFileExist c
    valueOrZeroM b (T.readFile c)
  firstEitherT ContextError (hoistEither (asWith contextFromJSON ctx))

bmxConcise :: Text -> [(Text, BMXValue)] -> EitherT CLIError IO ()
bmxConcise t c = firstEitherT TemplateError $ do
  tmp <- hoistEither (templateFromText t)
  res <- hoistEither (renderTemplate st tmp)
  liftIO $ T.putStrLn (renderPage res)
  where st = defaultState `usingContext` c

-- Verbose mode will print the context, all lexed tokens, and the
-- parsed AST (just like the old CLI used to do)
bmxSpam :: Text -> [(Text, BMXValue)] -> EitherT CLIError IO ()
bmxSpam t c = do
  printContext
  newLine
  printTokens
  newLine
  printAST
  newLine
  printResult
  where
    tokens = tokenise t
    template = templateFromText t
    --
    newLine = liftIO $ T.putStrLn T.empty
    printContext = liftIO $ T.putStrLn "Context:" >> T.putStrLn (asTextWith contextToJSON c)
    printTokens = liftIO $ T.putStrLn "Tokens:" >> print tokens
    printAST = liftIO $ T.putStrLn "AST:" >> either (const (return ())) print template
    printResult = liftIO (T.putStrLn "Result:") >> bmxConcise t c

-- -----------------------------------------------------------------------------
-- Arg parser

bmxP :: Parser BMXCommand
bmxP = versionP <|> evalP

versionP :: Parser BMXCommand
versionP = versionFlag *> pure BMXVersion

evalP :: Parser BMXCommand
evalP = BMXEval <$> verbP <*> contextP

verbP :: Parser Verbosity
verbP = flag Normal Spam (long "verbose")

contextP :: Parser (Maybe FilePath)
contextP = optional . strOption $
     short 'c'
  <> long "context"
  <> metavar "context.json"

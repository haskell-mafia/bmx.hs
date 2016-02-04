{- | Template Haskell splices to make life easier. -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module BMX.TH (
    bmx
  , templateFile
  , partialFile
  , partialDir
  ) where

import           Data.List (zipWith)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.FilePath (dropExtension)
import           System.FilePath.Find (find, always, extension, (==?))
import           System.IO (IO, FilePath, putStrLn)

import           Data.Data (Data)
import           Data.Generics (extQ)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (qAddDependentFile)
import           X.Language.Haskell.TH (qeither)
import qualified Prelude (error)

import           BMX.Data hiding (StringL)
import           BMX.Eval (partialFromTemplate)
import           BMX.Parser (templateFromText)

import           P hiding (find)


-- | Quasiquoter for building inline BMX templates and checking them at compile time.
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- >
-- > -- Build fails if this template doesn't parse.
-- > myTemplate :: Template
-- > myTemplate = [bmx|{{# if author }} {{author.name}} {{else}} nah {{/if}}|]
bmx :: QuasiQuoter
bmx = qeither (first renderBMXError . templateFromText)

dataExp :: Data a => a -> Q Exp
dataExp a = dataToExpQ (const Nothing `extQ` textExp) a
  where textExp = pure . appE (varE 'T.pack) . litE . StringL . T.unpack

-- | Template Haskell splice to load a 'Template' from disk.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > myTemplate :: Template
-- > myTemplate = $(templateFile "templates/login.hbs")
templateFile :: FilePath -> Q Exp
templateFile fp = do
  qAddDependentFile fp -- Will recompile module if file changes on disk
  t <- runIO (templateFile' fp)
  dataExp t

-- | Template Haskell splice to load a 'Partial' from disk.
--
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > myGoodPartial :: (Applicative m, Monad m) => Partial m
-- > myGoodPartial = $(partialFile "partials/realgood.hbs")
partialFile :: FilePath -> Q Exp
partialFile fp = do
  e <- templateFile fp
  return (AppE (VarE 'partialFromTemplate) e)

-- | Template Haskell splice to recursively load all partials from a directory
-- at compile time. Any partial with the @.hbs@ extension will be loaded, and
-- the relative name sans extension will be returned.
--
-- Suppose we have a directory in our project root named @partials@ like so:
--
-- > partials/
-- >   partial1.hbs
-- >   other/
-- >     other1.hbs
--
-- We can load it using 'partialDir' like so:
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > myPartials :: (Applicative m, Monad m) => [(Text, Partial m)]
-- > myPartials = $(partialDir "./partials")
--
-- ... producing partials named @"partials/partial1"@ and @"partials/other/other1"@.
partialDir :: FilePath -> Q Exp
partialDir fp = do
  files <- runIO (find always (extension ==? ".hbs") fp)
  mapM_ qAddDependentFile files
  exprs <- runIO (forM files templateFile')
  elist <- dataExp (zipWith (\name expr -> (packName name, expr)) files exprs)
  -- Inline 'fmap (fmap partialFromTemplate) elist'
  return (AppE (AppE (VarE 'fmap) (AppE (VarE 'fmap) (VarE 'partialFromTemplate))) elist)
  where packName = T.pack . dropExtension

templateFile' :: FilePath -> IO Template
templateFile' fp = do
  putStrLn $ "[BMX] Compiling template from file " <> fp
  text <- T.readFile fp
  case templateFromText text of
    Left b -> Prelude.error $ "Failed to parse template " <> fp <> ": " <> T.unpack (renderBMXError b)
    Right a -> return a

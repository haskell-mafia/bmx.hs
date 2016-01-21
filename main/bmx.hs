{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map.Strict as M
import qualified Data.Text.IO as T
import           System.IO

import           BMX
import           BMX.Data
import           BMX.Eval
import           BMX.Function

import           P

main :: IO ()
main = do
  inp <- T.getContents
  let parsed = templateFromText inp
  either (T.putStrLn . renderParseError) print parsed
  let scream = T.hPutStrLn stderr
      drawResult (epage, er) = do
        either (scream . renderEvalError) (T.putStrLn . renderPage) epage
        mapM_ (scream . renderEvalOutput) er
  either (const $ return ())
         (drawResult . renderTemplate testContext)
         parsed

testContext :: Context
testContext = Context $ M.fromList [
    ("title", StringV "My First Blog Post!")
  , ("author", ContextV . Context $ M.fromList [
                   ("id", IntV 47)
                 , ("name", StringV "Yehuda Katz")
                 ])
  , ("body", StringV "My first post. Wheeeee!")
  , ("html", StringV "<a href=\"google.com\">Cool Site</a>")
  , ("component", StringV "authorid")
  ]

-- FIX temporary test value
testPartial :: (Applicative m, Monad m) => Partial m
testPartial = Partial . eval $
  Template
    [ ContentStmt "The author's name is "
    , Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "name" Nothing)) [] (Hash []))
    , ContentStmt " and their ID is "
    , Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "id" Nothing)) [] (Hash []))
    , ContentStmt " arg = "
    , Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "arg" Nothing)) [] (Hash []))
    ]

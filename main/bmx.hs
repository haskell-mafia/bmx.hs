{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text (Text)
import qualified Data.Text.IO as T
import           System.IO

import           BMX
import           BMX.Internal

import           P

main :: IO ()
main = do
  inp <- T.getContents
  let parsed = templateFromText inp
  either (T.putStrLn . renderBMXError) print parsed
  let scream = T.hPutStrLn stderr
      drawResult epage =
        either (scream . renderBMXError) (T.putStrLn . renderPage) epage
  either (const $ return ())
         (drawResult . renderTemplate testState)
         parsed

testState :: (Applicative m, Monad m) => BMXState m
testState = defaultState
  `usingContext` testContext
  `usingPartials` [("authorid", partialFromTemplate testPartial)]

testContext :: [(Text, BMXValue)]
testContext = [
    ("title", BMXString "My First Blog Post!")
  , ("author", BMXContext [
                   ("id", BMXNum 47)
                 , ("name", BMXString "Yehuda Katz")
                 ])
  , ("body", BMXString "My first post. Wheeeee!")
  , ("html", BMXString "<a href=\"google.com\">Cool Site</a>")
  , ("component", BMXString "authorid")
  ]

testPartial :: Template
testPartial =
  Template
    [ ContentStmt "The author's name is "
    , Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "name" Nothing)) [] (Hash []))
    , ContentStmt " and their ID is "
    , Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "id" Nothing)) [] (Hash []))
    , ContentStmt " arg = "
    , Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "arg" Nothing)) [] (Hash []))
    ]

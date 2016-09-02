{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import           BMX
import           BMX.Lexer (tokenise)

import           Criterion.Main
import           Criterion.Types (Config (..))

import           Data.List ((!!), iterate)
import qualified Data.Text as T

import           P

import qualified Prelude

import           System.IO (IO)

import           Test.BMX.Orphans ()


flatTemplate :: Int -> Template
flatTemplate n = big
  where
    big = iterate (mappend templat) mempty !! n
    templat = [bmx|
      <div class="post">
        <h1>By {{author.firstName}} {{author.lastName}}</h1>
        <div class="body">{{body}}</div>

        <h1>Comments</h1>

        {{#each comments}}
          <h2>By {{author.firstName}} {{author.lastName}}</h2>
          <div class="body">{{body}}</div>
        {{/each}}

        {{#> partial comments.[0] }}
          <h2>By {{author.firstName}} {{author.lastName}}</h2>
          <div class="body">{{body}}</div>
        {{/partial}}
      </div>
    |]

flatTemplateTextStrict :: Int -> Text
flatTemplateTextStrict = force . templateToText . flatTemplate

flatRenderer :: Template -> Text
flatRenderer = force . either (Prelude.error . T.unpack . renderBMXError) renderPage . renderTemplate env'
  where
    env' = defaultState
      `usingPartials` [
           ("partial", partialFromTemplate [bmx|{{author.firstName}} {{author.lastName}} {{body}}|])
         ]
      `usingContext` [
           ("author", BMXContext [
               ("firstName", BMXString "Alan")
             , ("lastName", BMXString "Johnson")
             ])
         , ("body", BMXString "I Love Handlebars")
         , ("comments", BMXList [
               BMXContext [
                   ("author", BMXContext [
                       ("firstName", BMXString "Yehuda")
                     , ("lastName", BMXString "Katz")
                     ])
                 , ("body", BMXString "Me too!")
                 ]
             ])
         ]

main :: IO ()
main = do

  let cfg =
        defaultConfig {
            reportFile = Just "dist/build/bmx.hs-bench.html"
          , csvFile    = Just "dist/build/bmx.hs-bench.csv"
          }
      flatLexBm = whnf tokenise . flatTemplateTextStrict
      flatParseBm = whnf templateFromText . flatTemplateTextStrict
      flatRenderBm = whnf flatRenderer . flatTemplate

  defaultMainWith cfg [
      bgroup "tokenise" [
          bench "flat-100"    $ flatLexBm 100
        , bench "flat-500"    $ flatLexBm 500
        , bench "flat-1000"   $ flatLexBm 1000
        ]
    , bgroup "templateFromText" [
          bench "flat-100"    $ flatParseBm 100
        , bench "flat-500"    $ flatParseBm 500
        , bench "flat-1000"   $ flatParseBm 1000
        ]
    , bgroup "renderTemplate" [
          bench "flat-100"    $ flatRenderBm 100
        , bench "flat-500"    $ flatRenderBm 500
        , bench "flat-1000"   $ flatRenderBm 1000
        ]
    ]

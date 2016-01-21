{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.BMX.Eval where

import           Control.Monad.Identity (Identity)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as T (toStrict)
import           Test.QuickCheck
import qualified Text.Blaze.Html as B
import qualified Text.Blaze.Html.Renderer.Text as B

import           BMX
import           BMX.Builtin
import           BMX.Data
import           BMX.Eval (eval)
import           BMX.Function

import           Test.BMX.Arbitrary

import           P

rendersTo :: Text -> Context -> Either Text Text
rendersTo input ctx = flatten (renderTemplate ctx) (templateFromText input)

-- FIX this should be replaced by some public interface eventually
rendersWithPartialsTo :: Text -> Context -> Map Text (Partial Identity) -> Either Text Text
rendersWithPartialsTo input ctx partials = flatten renderIt (templateFromText input)
  where renderIt = runBMX ((defaultState ctx) { evalPartials = partials }) . eval

flatten f = bimap renderEvalError id . either
  (Left . ParserError . renderParseError)
  (fmap renderPage . fst . f)

escapeText = T.toStrict . B.renderHtml . B.toHtml

tshow = T.pack . show
mustache t = "{{" <> t <> "}}"
single n v = Context $ M.fromList [(n, v)]

-- -----------------------------------------------------------------------------
-- Mustache tests (no helpers)

-- any valid content should render to itself
prop_eval_content_id t = validContent t ==>
  rendersTo t mempty === pure t

-- any valid string invoked in a mustache should render to itself, escaped
prop_eval_mustache_string t = validString t ==> forAll simpleId $ \n ->
  rendersTo (mustache n) (single n (StringV t)) === pure (escapeText t)

-- same for unescaped mustache
prop_eval_mustache_string_unescaped t = validString t ==> forAll simpleId $ \n ->
  rendersTo ("{{{" <> n <> "}}}") (single n (StringV t)) === pure t

-- any valid number invoked in a mustache should render to itself
prop_eval_mustache_number i = forAll simpleId $ \n ->
  rendersTo (mustache n) (single n (IntV i)) === pure (tshow i)

-- both bools
prop_eval_mustache_bool_true = forAll simpleId $ \n ->
  rendersTo (mustache n) (single n (BoolV True)) === pure "true"

prop_eval_mustache_bool_false = forAll simpleId $ \n ->
  rendersTo (mustache n) (single n (BoolV False)) === pure "false"

-- should not be able to render undefined, null, context or list
prop_eval_mustache_null_fails = forAll simpleId $ \n ->
  isLeft (rendersTo (mustache n) (single n NullV))

prop_eval_mustache_undef_fails = forAll simpleId $ \n ->
  isLeft (rendersTo (mustache n) (single n UndefinedV))

prop_eval_mustache_context_fails ctx = forAll simpleId $ \n ->
  isLeft (rendersTo (mustache n) (single n (ContextV ctx)))

prop_eval_mustache_list_fails ls = forAll simpleId $ \n ->
  isLeft (rendersTo (mustache n) (single n (ListV ls)))

-- unknown variables in mustaches should throw errors
prop_eval_mustache_undefined_var_fails = forAll simpleId $ \n ->
  isLeft (rendersTo (mustache n) mempty)

-- comments should go away
prop_eval_comment_elim_1 t ctx = validComment t ==>
  rendersTo ("{{!--" <> t <> "--}}") ctx === pure T.empty

prop_eval_comment_elim_2 t ctx = validWeakComment t ==>
  rendersTo ("{{!" <> t <> "}}") ctx === pure T.empty

-- -----------------------------------------------------------------------------
-- Partials

-- should throw an error when using an undefined partial
prop_eval_partial_fails = forAll simpleId $ \n ->
  isLeft (rendersTo ("{{>" <> n <> "}}") mempty)

-- if block partial not found, should fail over to intermediate content
-- FIX replace content with a generated valid block when generator written
prop_eval_block_partial_failover t = validContent t ==> forAll simpleId $ \n ->
  rendersTo ("{{#>" <> n <> "}}" <> t <> "{{/" <> n <> "}}") mempty === pure t

-- -----------------------------------------------------------------------------
-- Decorators

-- define an inline partial containing some content, and immediately invoke it
prop_eval_inline_content t = validContent t ==> forAll simpleId $ \n ->
  rendersTo ("{{#* inline '" <> n <> "' }}" <> t <> "{{/inline}}{{>" <> n <> "}}") testContext === pure t

-- -----------------------------------------------------------------------------
-- regression tests / units
-- FIX can turn these into properties once Context interface is nicer / have a good generator

prop_eval_unit_if = once $
  rendersTo "{{# if author }}{{author.name}}{{/if}}" testContext === pure "Yehuda Katz"

prop_eval_unit_if_else = once $
  rendersTo "{{# if garbage }}{{garbage}}{{else}}{{title}}{{/if}}" testContext
    === pure "My First Blog Post!"

prop_eval_unit_if_else_chain_1 = once $
  rendersTo
    "{{# if garbage }}{{garbage}}{{else if author}}{{author.id}}{{else}}{{title}}{{/if}}"
    testContext
    === pure "47"

prop_eval_unit_if_else_chain_2 = once $
  rendersTo
    "{{# if garbage }}{{garbage}}{{else if doctorb}}{{doctorb.id}}{{else}}{{title}}{{/if}}"
    testContext
    === pure "My First Blog Post!"

prop_eval_unit_each_ctx = once $
  rendersTo "{{# each author }}{{@index}} {{@key}}: {{this}} {{/each}}" testContext
    === pure "0 id: 47 1 name: Yehuda Katz "

prop_eval_unit_each_ctx_named = once $
  rendersTo "{{# each author as |field idx|}}{{idx}} {{field}} {{/each}}" testContext
    === pure "0 47 1 Yehuda Katz "

prop_eval_unit_each_list = once $
  rendersTo "{{# each list }}{{@index}} {{this}} {{/each}}" testContext
    === pure "0 why 1 not? "

prop_eval_unit_each_list_named = once $
  rendersTo "{{# each list as |item idx|}}{{item}} {{/each}}" testContext
    === pure "why not? "

prop_eval_unit_each_first_last = once $
  rendersTo "{{# each list }}{{#if @first}}first! {{/if}}{{#if @last}}last!{{/if}}{{/each}}" testContext
    === pure "first! last!"

-- lookup in current context
prop_eval_unit_lookup_1 = once $
  rendersTo "{{ lookup . 'title' }}" testContext === pure "My First Blog Post!"

-- lookup in another context
prop_eval_unit_lookup_2 = once $
  rendersTo "{{ lookup author 'name' }}" testContext === pure "Yehuda Katz"

-- lookup something we set in the hash - this should work
prop_eval_unit_lookup_3 = once $
  rendersTo "{{ lookup . 'abcde' abcde=100 }}" testContext === pure "100"

-- regular partial
prop_eval_unit_partial = once $
  rendersWithPartialsTo "{{> mypartial }}" testContext
    (M.fromList [("mypartial", simplePartial)])
    === pure "They got those chewy pretzels"

-- partial with custom context
prop_eval_unit_partial_ctx = once $
  rendersWithPartialsTo "{{> authorid author }}" testContext
    (M.fromList [("authorid", testPartial')])
    === pure "The author's name is Yehuda Katz and their ID is 47"

-- partial with custom context and a hash
prop_eval_unit_partial_hash = once $
  rendersWithPartialsTo "{{> authorid author arg=500}}" testContext
    (M.fromList [("authorid", testPartial)])
    === pure "The author's name is Yehuda Katz and their ID is 47 arg = 500"

-- a dynamic partial with custom context and a hash
prop_eval_unit_partial_dynamic = once $
  rendersWithPartialsTo "{{> (lookup . 'component') author arg=555}}" testContext
    (M.fromList [("authorid", testPartial)])
    === pure "The author's name is Yehuda Katz and their ID is 47 arg = 555"

-- a partial invoked as partial block can access @partial-block special variable
prop_eval_unit_partial_block_data = once $
  rendersWithPartialsTo "{{#> mypartial }}{{title}}{{/mypartial}}" testContext
    (M.fromList [("mypartial", testPartialBlock)])
    === pure "block = My First Blog Post!"

prop_eval_unit_list_index = once $
  rendersTo "{{ list.[0] }}" testContext === pure "why"

testContext :: Context
testContext = Context $ M.fromList [
    ("title", StringV "My First Blog Post!")
  , ("author", ContextV . Context $ M.fromList [
                   ("id", IntV 47)
                 , ("name", StringV "Yehuda Katz")
                 ])
  , ("list", ListV [
                 StringV "why"
               , StringV "not?"
               ])
  , ("body", StringV "My first post. Wheeeee!")
  , ("html", StringV "<a href=\"google.com\">Cool Site</a>")
  , ("component", StringV "authorid")
  ]

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

testPartial' :: (Applicative m, Monad m) => Partial m
testPartial' = Partial . eval $
  Template
    [ ContentStmt "The author's name is "
    , Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "name" Nothing)) [] (Hash []))
    , ContentStmt " and their ID is "
    , Mustache (Fmt Verbatim Verbatim) (SExp (PathL (PathID "id" Nothing)) [] (Hash []))
    ]

simplePartial :: (Applicative m, Monad m) => Partial m
simplePartial = Partial . eval $
  Template
    [ ContentStmt "They got those chewy pretzels"
    ]

testPartialBlock :: (Applicative m, Monad m) => Partial m
testPartialBlock = Partial . eval $
  Template
    [ ContentStmt "block = "
    , PartialStmt (Fmt Verbatim Verbatim) (Lit (DataL (DataPath (PathID "partial-block" Nothing)))) Nothing (Hash [])
    ]

-- -----------------------------------------------------------------------------

return []
tests = $quickCheckAll

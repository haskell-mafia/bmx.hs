import           Disorder.Core.Main

import           Test.BMX.Eval as Eval (tests)
import           Test.BMX.Function as Function (tests)
import           Test.BMX.Lexer as Lexer (tests)
import           Test.BMX.Parser as Parser (tests)
import           Test.BMX.Page as Page (tests)
import           Test.BMX.Position as Position
import           Test.BMX.TH as TH (tests)

main :: IO ()
main =
  disorderMain [
     -- Handlebars language spec
       Lexer.tests
     , Parser.tests
     , Position.tests
     -- BMX
     , Page.tests
     , Function.tests
     , Eval.tests
     , TH.tests
    ]

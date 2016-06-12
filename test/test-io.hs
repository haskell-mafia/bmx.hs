import           Disorder.Core.Main

import qualified Test.IO.BMX.React

main :: IO ()
main =
  disorderMain [
       Test.IO.BMX.React.tests
    ]


import           Criterion.Main
import           Criterion.Types
import           Data.List
import           Data.Monoid

import           Data.Crdt.TreeVector

main :: IO ()
main = defaultMainWith config $
  bench "testStrings" (nf testDocument testStrings) :
  []
  where
    config = defaultConfig{
      reportFile = Just "benchmarks.html"
    }

testStrings :: [String]
testStrings =
  "hello" :
  "hello world" :
  "hello new world" :
  "brave new world" :
  []

testDocument :: [String] -> TreeVector Int Char
testDocument docs =
  foldl' (\ doc new -> doc <> mkPatch (Client 0) doc new) mempty docs

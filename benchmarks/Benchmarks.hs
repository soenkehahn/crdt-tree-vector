
import           Criterion.Main
import           Criterion.Types
import           Data.List
import           Data.Monoid

import           Data.Crdt.TreeVector

main :: IO ()
main = defaultMainWith config $
  bench "updates" (nf mkTestDocument testStrings) :
  bench "merge with the same document" (nf (\ a -> a <> a) testDocument) :
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

mkTestDocument :: [String] -> TreeVector Int Char
mkTestDocument docs =
  foldl' (\ doc new -> doc <> mkPatch (Client 0) doc new) mempty docs

testDocument :: TreeVector Int Char
testDocument = mkTestDocument testStrings

{-# LANGUAGE ScopedTypeVariables #-}

module Data.Crdt.TreeVector.CursorSpec (spec) where

import           Control.DeepSeq
import           Control.Exception
import           Data.List
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Data.Crdt.TreeVector
import           Data.Crdt.TreeVector.Cursor
import           Data.Crdt.TreeVector.InternalSpec (mkClient)
import           Data.Crdt.TreeVector.Pretty

spec :: Spec
spec = do
  modifyMaxSize (min 50) $ do
    describe "Cursor" $ do
      it "stays the same when nothing gets merged" $
        forAllDocs $ \ doc ->
        forAllIndices doc $ \ index ->
          let cursor = toCursor doc index
              newIndex = fromCursor doc cursor in
          counterexample (show cursor) $
          newIndex === index

      context "when an element gets inserted" $ do
        it "moves the index on an earlier insert" $ do
          let original = "foobar"
              doc = mkPatch (mkClient 0) mempty original
              cursor = toCursor doc 3
              newDoc = mkPatch (mkClient 0) doc "fxoobar"
              newCursor = fromCursor newDoc cursor
          newCursor `shouldBe` 4

        it "retains the index on a later insert" $ do
          let original = "foobar"
              doc = mkPatch (mkClient 0) mempty original
              cursor = toCursor doc 3
              newDoc = mkPatch (mkClient 0) doc "foobaxr"
              newCursor = fromCursor newDoc cursor
          newCursor `shouldBe` 3

        it "retains the index if an element gets inserted at the cursor's position" $ do
          let original = "foobar"
              doc = mkPatch (mkClient 0) mempty original
              cursor = toCursor doc 3
              newDoc = mkPatch (mkClient 1) doc "fooxbar"
              newCursor = fromCursor newDoc cursor
          newCursor `shouldBe` 4 -- fixme

      context "when an element gets deleted" $ do
        it "gets decreased on earlier deletions" $ do
          let original = "fooxbar"
              doc = mkPatch (mkClient 0) mempty original
              cursor = toCursor doc 5
              newDoc = mkPatch (mkClient 0) doc "foobar"
              newCursor = fromCursor newDoc cursor
          newCursor `shouldBe` 4

        it "stays the same on later deletions" $ do
          let original = "fooxbar"
              doc = mkPatch (mkClient 0) mempty original
              cursor = toCursor doc 1
              newDoc = mkPatch (mkClient 0) doc "foobar"
              newCursor = fromCursor newDoc cursor
          newCursor `shouldBe` 1

        it "works when it points to an element that gets deleted" $ do
          let original = "fooxbar"
              doc = mkPatch (mkClient 0) mempty original
              cursor = toCursor doc 3
              newDoc = mkPatch (mkClient 0) doc "foobar"
              newCursor = fromCursor newDoc cursor
          newCursor `shouldBe` 3

      it "allows to set the cursor for an empty document" $ do
        let empty = mempty :: TreeVector Int Char
            cursor = toCursor empty 0
        fromCursor empty cursor `shouldBe` 0

      it "allows to set the cursor after the last element" $ do
        let doc = mkPatch (mkClient 0) mempty "foo"
            cursor = toCursor doc 3
        fromCursor doc cursor `shouldBe` 3

      it "allows to set the cursor before the first element" $ do
        let doc = mkPatch (mkClient 0) mempty "foo"
            cursor = toCursor doc 0
        fromCursor doc cursor `shouldBe` 0

      it "is total" $
        forAllDocs $ \ a ->
        forAllDocs $ \ b -> \ index -> do
          evaluate $ rnf $ fromCursor b $ toCursor a index

forAllDocs :: Testable t => (TreeVector Int Char -> t) -> Property
forAllDocs prop = property $ \ (vectors :: [String]) ->
  let doc = foldl' (\ doc new -> mkPatch (mkClient 0) doc new) mempty vectors
  in counterexample (ppTree doc) $
    prop doc

forAllIndices :: Testable t => TreeVector Int a -> (Int -> t) -> Property
forAllIndices doc prop =
  forAllShrink gen shr prop
  where
    gen :: Gen Int
    gen = choose (0, length (getVector doc))

    shr :: Int -> [Int]
    shr = filter (\ i -> i >= 0 && i <= length (getVector doc)) . shrink

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module CRDT.TreeVector.InternalSpec where

import           Prelude ()
import           Prelude.Compat

import           "quickcheck-instances" Test.QuickCheck.Instances ()
import           Data.Proxy
import           Data.Semigroup hiding (diff)
import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           CRDT.Class
import           CRDT.TreeVector ()
import           CRDT.TreeVector.Internal
import           Test.Utils

spec :: Spec
spec = do
  isSemilattice (Proxy :: Proxy (Element Char))
  isSemilattice (Proxy :: Proxy (Node Char))

  describe "TreeVector" $ do
    isCRDT (Proxy :: Proxy (TreeVector Char))
    testBatch (monoid (undefined :: TreeVector Char))

    it "joins are commutative" $ do
      property $ \ (a :: TreeVector Char) b ->
        a <> b === b <> a

  describe "mkPatch" $ do
    it "can create concurrent patches for two clients" $ do
      let initial = mempty :: TreeVector Char
          patchA = mkPatch (Client 1) initial "abc"
          patchB = mkPatch (Client 2) initial "xyz"
      query (patchA <> patchB) `shouldBe` "abcxyz"

    it "can create concurrent deletions for two clients" $ do
      let initial = mempty :: TreeVector Char
          patchA = mkPatch (Client 1) initial "abc"
          patchB = mkPatch (Client 1) patchA "ab"
          patchC = mkPatch (Client 2) initial "xyz"
      query (patchB <> patchC) `shouldBe` "abxyz"

    it "can create concurrent deletions for two clients" $ do
      let initial = mempty :: TreeVector Char
          patchA = mkPatch (Client 1) initial "abc"
          patchB = mkPatch (Client 2) initial "xyz"
          patchC = patchA <> patchB
          patchD = mkPatch (Client 1) patchC "abxz"
      query (patchB <> patchD) `shouldBe` "abxz"

    it "can create patch documents" $ do
      property $ \ (tree :: TreeVector Char) s ->
        counterexample (show (query tree)) $
        counterexample (show (diff (query tree) s)) $
        query (tree <> mkPatch (Client 1) tree s) === s

instance Arbitrary (Element Char) where
  arbitrary = oneof $
    (Set <$> arbitrary) :
    pure Deleted :
    []
  shrink Deleted = map Set ['a' .. 'z']
  shrink (Set c) = map Set $ shrink c

instance EqProp (Element Char) where
  a =-= b = get a === get b

instance Arbitrary Client where
  arbitrary = Client <$> elements [0 .. 10]
  shrink (Client c) = map Client $ shrink c

instance Arbitrary (Node Char) where
  arbitrary = scale (`div` 9)
    (Node <$> arbitrary <*> arbitrary <*> arbitrary)
  shrink (Node l c r) =
    (map (\ l' -> Node l' c r) (shrink l)) ++
    (map (\ c' -> Node l c' r) (shrink c)) ++
    (map (\ r' -> Node l c r') (shrink r))

instance EqProp (Node Char) where
  a =-= b = getNodeVector a === getNodeVector b

instance Arbitrary (TreeVector Char) where
  arbitrary = TreeVector <$> arbitrary
  shrink (TreeVector m) = map TreeVector $ shrink m

instance EqProp (TreeVector Char) where
  a =-= b = query a === query b

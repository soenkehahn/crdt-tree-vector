{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Crdt.TreeVector.InternalSpec where

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

import           Data.Crdt.TreeVector ()
import           Data.Crdt.TreeVector.Internal
import           Test.Utils

spec :: Spec
spec = do
  isSemilattice (Proxy :: Proxy (Element Char))
  isSemilattice (Proxy :: Proxy (Node Int Char))

  describe "TreeVector" $ do
    isSemilattice (Proxy :: Proxy (TreeVector Int Char))
    testBatch (monoid (undefined :: TreeVector Int Char))

    it "joins are commutative" $ do
      property $ \ (a :: TreeVector Int Char) b ->
        a <> b === b <> a

  describe "mkPatch" $ do
    it "can create concurrent patches for two clients" $ do
      let initial = mempty
          patchA = mkPatch (mkClient 1) initial "abc"
          patchB = mkPatch (mkClient 2) initial "xyz"
      getVector (patchA <> patchB) `shouldBe` "abcxyz"

    it "can create concurrent deletions for two clients" $ do
      let initial = mempty
          patchA = mkPatch (mkClient 1) initial "abc"
          patchB = mkPatch (mkClient 1) patchA "ab"
          patchC = mkPatch (mkClient 2) initial "xyz"
      getVector (patchB <> patchC) `shouldBe` "abxyz"

    it "can create concurrent deletions for two clients" $ do
      let initial = mempty
          patchA = mkPatch (mkClient 1) initial "abc"
          patchB = mkPatch (mkClient 2) initial "xyz"
          patchC = patchA <> patchB
          patchD = mkPatch (mkClient 1) patchC "abxz"
      getVector (patchB <> patchD) `shouldBe` "abxz"

    it "can create patch documents" $ do
      property $ \ (tree :: TreeVector Int Char) s ->
        counterexample (show (getVector tree)) $
        counterexample (show (diff (getVector tree) s)) $
        getVector (tree <> mkPatch (mkClient 1) tree s) === s

instance Arbitrary (Element Char) where
  arbitrary = oneof $
    (Set <$> arbitrary) :
    pure Deleted :
    []
  shrink Deleted = map Set ['a' .. 'z']
  shrink (Set c) = map Set $ shrink c

instance EqProp (Element Char) where
  a =-= b = get a === get b

mkClient :: Int -> Client Int
mkClient = Client

instance Arbitrary (Client Int) where
  arbitrary = Client <$> elements [0 .. 10]
  shrink (Client c) = map Client $ shrink c

instance Arbitrary (Node Int Char) where
  arbitrary = scale (`div` 9)
    (Node <$> arbitrary <*> arbitrary <*> arbitrary)
  shrink (Node l c r) =
    (map (\ l' -> Node l' c r) (shrink l)) ++
    (map (\ c' -> Node l c' r) (shrink c)) ++
    (map (\ r' -> Node l c r') (shrink r))

instance EqProp (Node Int Char) where
  a =-= b = getNodeVector a === getNodeVector b

instance Arbitrary (TreeVector Int Char) where
  arbitrary = TreeVector <$> arbitrary
  shrink (TreeVector m) = map TreeVector $ shrink m

instance EqProp (TreeVector Int Char) where
  a =-= b = getVector a === getVector b

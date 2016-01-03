{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CRDT.TreeVector.InternalSpec where

import           "quickcheck-instances" Test.QuickCheck.Instances
import           Data.Proxy
import           Data.Semigroup hiding (diff)
import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

import           CRDT.TreeVector.Internal
import           Test.Utils

spec :: Spec
spec = do
  isSemigroup (Proxy :: Proxy DChar)
  isSemigroup (Proxy :: Proxy Node)

  describe "Tree" $ do
    isSemigroup (Proxy :: Proxy Tree)
    testBatch (monoid (undefined :: Tree))

    it "joins are commutative" $ do
      property $ \ (a :: Tree) b ->
        a <> b === b <> a

  describe "update" $ do
    it "can create concurrent patches for two clients" $ do
      let initial = mempty
          patchA = update (Client 1) initial "abc"
          patchB = update (Client 2) initial "xyz"
      getDocument (patchA <> patchB) `shouldBe` "abcxyz"

    it "can create concurrent deletions for two clients" $ do
      let initial = mempty
          patchA = update (Client 1) initial "abc"
          patchB = update (Client 1) patchA "ab"
          patchC = update (Client 2) initial "xyz"
      getDocument (patchB <> patchC) `shouldBe` "abxyz"

    it "can create concurrent deletions for two clients" $ do
      let initial = mempty
          patchA = update (Client 1) initial "abc"
          patchB = update (Client 2) initial "xyz"
          patchC = patchA <> patchB
          patchD = update (Client 1) patchC "abxz"
      getDocument (patchB <> patchD) `shouldBe` "abxz"

    it "can create patch documents" $ do
      property $ \ tree s ->
        counterexample (show (getDocument tree)) $
        counterexample (show (diff tree s)) $
        getDocument (tree <> update (Client 1) tree s) === s

instance Arbitrary DChar where
  arbitrary = oneof $
    (DChar <$> arbitrary) :
    pure Deleted :
    []
  shrink Deleted = map DChar ['a' .. 'z']
  shrink (DChar c) = map DChar $ shrink c

instance EqProp DChar where
  a =-= b = get a === get b

instance Arbitrary Client where
  arbitrary = Client <$> elements [0 .. 10]
  shrink (Client c) = map Client $ shrink c

instance Arbitrary Node where
  arbitrary = scale (`div` 9)
    (Node <$> arbitrary <*> arbitrary <*> arbitrary)
  shrink (Node l c r) =
    (map (\ l' -> Node l' c r) (shrink l)) ++
    (map (\ c' -> Node l c' r) (shrink c)) ++
    (map (\ r' -> Node l c r') (shrink r))

instance EqProp Node where
  a =-= b = getNodeDoc a === getNodeDoc b

instance Arbitrary Tree where
  arbitrary = Tree <$> arbitrary
  shrink (Tree m) = map Tree $ shrink m

instance EqProp Tree where
  a =-= b = getDocument a === getDocument b

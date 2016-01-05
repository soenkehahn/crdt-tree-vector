{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Utils where

import           Data.List
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Set as Set
import           Data.Typeable
import           Test.Hspec
import           Test.QuickCheck

import           CRDT.Class

isSemigroup :: forall a . (Eq a, Show a, Typeable a, Arbitrary a, Semigroup a) =>
  Proxy a -> Spec
isSemigroup proxy = do
  describe (show (typeRep proxy) ++ " is a lawful Semigroup") $ do
    associativity proxy

isSemilattice :: forall a . (Eq a, Show a, Typeable a, Arbitrary a, Semigroup a) =>
  Proxy a -> Spec
isSemilattice proxy = do
  describe (show (typeRep proxy) ++ " is a lawful Semilattice") $ do
    associativity proxy
    idempotency proxy

isCRDT :: (Ord a, Eq (External a), Show a, Show (External a), Typeable a, Arbitrary a, Arbitrary (External a), CRDT a) =>
  Proxy a -> Spec
isCRDT proxy = do
  describe (show (typeRep proxy) ++ " is a lawful CRDT") $ do
    monotonicity proxy
    mkPatchCommutes proxy
    associativity proxy
    commutativity proxy
    idempotency proxy

associativity :: forall a . (Eq a, Show a, Arbitrary a, Semigroup a) =>
  Proxy a -> Spec
associativity Proxy = do
  it "associativity" $
    property $ \ a b (c :: a) ->
      (a <> b) <> c === a <> (b <> c)

idempotency :: forall a . (Eq a, Show a, Arbitrary a, Semigroup a) =>
  Proxy a -> Spec
idempotency Proxy = do
  it "idempotency" $
    property $ \ (a :: a) ->
      a === a <> a

monotonicity :: forall a . (Ord a, Show a, Show (External a), Arbitrary a, Arbitrary (External a), CRDT a) =>
  Proxy a -> Spec
monotonicity Proxy = do
  context "monotonicity" $ do
    it "update is monotone" $ do
      mapSize (min 10) $ \ (a :: a) (bs :: [External a]) ->
        not $ circular (foldToList (update (Client 1)) a bs)

    it "(<>) is monotone" $ do
      mapSize (min 10) $ \ (a :: a) (bs :: [a]) ->
        not $ circular (foldToList (<>) a bs)

foldToList :: (a -> b -> a) -> a -> [b] -> [a]
foldToList f first (b : bs) = first : foldToList f (f first b) bs
foldToList _ last [] = [last]

circular :: Ord a => [a] -> Bool
circular (map head . group -> list) =
  length list /= length (ordNub list)

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

commutativity :: forall a . (Eq a, Show a, Arbitrary a, Semigroup a) =>
  Proxy a -> Spec
commutativity Proxy = do
  it "commutativity" $ do
    property $ \ (a :: a) b ->
      a <> b === b <> a

mkPatchCommutes :: forall a . (Eq (External a), Show a, Show (External a), Arbitrary a, Arbitrary (External a), CRDT a) =>
  Proxy a -> Spec
mkPatchCommutes Proxy = do
  it "mkPatchCommutes" $ do
    property $ \ (a :: a) b ->
      query (update (Client 1) a b) === b

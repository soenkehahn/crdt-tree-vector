{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Utils where

import           Data.Proxy
import           Data.Semigroup
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

isCRDT :: (Eq a, Eq (External a), Show a, Show (External a), Typeable a, Arbitrary a, CRDT a) =>
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

monotonicity :: forall a . (Show a, Arbitrary a, CRDT a) =>
  Proxy a -> Spec
monotonicity Proxy = do
  context "monotonicity" $ do
    it "update is monotone" $ do
      property $ \ (a :: a) (b :: a) ->
        let newExternal = query b
        in score (update (Client 1) a newExternal) >= score a

    it "(<>) is monotone" $ do
      property $ \ (a :: a) b ->
        score a <= score (a <> b)

commutativity :: forall a . (Eq a, Show a, Arbitrary a, Semigroup a) =>
  Proxy a -> Spec
commutativity Proxy = do
  it "commutativity" $ do
    property $ \ (a :: a) b ->
      a <> b === b <> a

mkPatchCommutes :: forall a . (Eq (External a), Show a, Show (External a), Arbitrary a, CRDT a) =>
  Proxy a -> Spec
mkPatchCommutes Proxy = do
  it "mkPatchCommutes" $ do
    property $ \ (a :: a) (b :: a) ->
      let newExternal = query b
          newInternal = update (Client 1) a newExternal
      in query newInternal === newExternal

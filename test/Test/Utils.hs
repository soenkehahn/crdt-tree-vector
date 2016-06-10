{-# LANGUAGE ScopedTypeVariables #-}

module Test.Utils where

import           Data.Proxy
import           Data.Semigroup
import           Data.Typeable
import           Test.Hspec
import           Test.QuickCheck

isSemilattice :: forall a . (Eq a, Show a, Typeable a, Arbitrary a, Semigroup a) =>
  Proxy a -> Spec
isSemilattice proxy = do
  describe (show (typeRep proxy) ++ " is a lawful Semilattice") $ do
    associativity proxy
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

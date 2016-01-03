{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import           Data.Proxy
import           Data.Semigroup
import           Data.Typeable
import           Test.Hspec
import           Test.QuickCheck

isSemigroup :: forall a . (Eq a, Show a, Typeable a, Arbitrary a, Semigroup a) =>
  Proxy a -> Spec
isSemigroup proxy = do
  describe (show (typeRep proxy) ++ " is a lawful Semigroup") $ do
    it "associativity" $
      property $ \ a b (c :: a) ->
        (a <> b) <> c === a <> (b <> c)

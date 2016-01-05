{-# LANGUAGE ScopedTypeVariables #-}

module Test.UtilsSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Test.Utils

spec :: Spec
spec = do
  describe "circular" $ do
    it "detects circular lists" $ do
      property $ \ (start :: String) c middle end ->
        not (null middle) ==>
        not (all (== c) middle) ==>
        let test = start ++ [c] ++ middle ++ [c] ++ end in
        counterexample ("should be circular: " ++ test) $
        circular test

    it "doesn't report repeated elements as circular" $ do
      circular "aa" `shouldBe` False
      circular "abbc" `shouldBe` False

    it "doesn't crash on empty lists" $ do
      circular "" `shouldBe` False

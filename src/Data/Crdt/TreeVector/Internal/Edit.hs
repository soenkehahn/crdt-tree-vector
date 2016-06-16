{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Crdt.TreeVector.Internal.Edit (
  Edit(..),
  index,
  modIndex,
  Data.Crdt.TreeVector.Internal.Edit.diff,
) where

import           Data.Monoid
import           Data.Vector
import           Data.Vector.Distance
import           GHC.Generics

data Edit a
  = Insert Int a
  | Delete Int
  deriving (Show, Eq, Generic)

index :: Edit a -> Int
index = \ case
  Insert i _ -> i
  Delete i -> i

modIndex :: (Int -> Int) -> Edit a -> Edit a
modIndex f = \ case
  Insert i c -> Insert (f i) c
  Delete i -> Delete (f i)

diff :: forall a . Eq a => [a] -> [a] -> [Edit a]
diff a b = Prelude.concat $ snd $
  leastChanges params (fromList a) (fromList b)
  where
    params :: Params a [Edit a] (Sum Int)
    params = Params {
      equivalent = (==),
      delete = \ i _v -> [Delete i],
      insert = \ i new -> [Insert i new],
      substitute = \ i _old new -> [Delete i, Insert i new],
      cost = \ _ -> Sum 1,
      positionOffset = \ case
        [Delete _] -> 0
        _ -> 1
    }

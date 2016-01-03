{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module CRDT.TreeVector.Internal.Edit (
  Edit(..),
  index,
  modIndex,
  CRDT.TreeVector.Internal.Edit.diff,
) where

import qualified Data.Patch as P
import qualified Data.Vector as V
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

diff :: Eq a => [a] -> [a] -> [Edit a]
diff a b =
  updateIndices 0 $ P.toList $
  P.diff (V.fromList a) (V.fromList b)
  where
    updateIndices offset (P.Insert i c : r) =
      Insert (offset + i) c : updateIndices (succ offset) r
    updateIndices offset (P.Delete i _ : r) =
      Delete (offset + i) : updateIndices (pred offset) r
    updateIndices offset (P.Replace i _ new : r) =
      Delete (offset + i) : Insert (offset + i) new : updateIndices offset r
    updateIndices _ [] = []

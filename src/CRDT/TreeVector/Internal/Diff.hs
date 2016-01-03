{-# LANGUAGE LambdaCase #-}

module CRDT.TreeVector.Internal.Diff (
  Edit(..),
  index,
  modIndex,
  CRDT.TreeVector.Internal.Diff.diff,
) where

import           Data.Patch hiding (index)
import qualified Data.Vector as V

diff :: String -> String -> [Edit Char]
diff tree s =
  updateIndices 0 $ toList $
  Data.Patch.diff (V.fromList tree) (V.fromList s)
  where
    updateIndices offset (Insert i c : r) =
      Insert (offset + i) c : updateIndices (succ offset) r
    updateIndices offset (Delete i old : r) =
      Delete (offset + i) old : updateIndices (pred offset) r
    updateIndices offset (Replace i old new : r) =
      Delete (offset + i) old : Insert (offset + i) new : updateIndices offset r
    updateIndices _ [] = []

modIndex :: (Int -> Int) -> Edit a -> Edit a
modIndex f = \ case
  Insert i c -> Insert (f i) c
  Delete i c -> Delete (f i) c
  Replace i old new -> Replace (f i) old new

index :: Edit a -> Int
index = \ case
  Insert i _ -> i
  Delete i _ -> i
  Replace i _ _ -> i

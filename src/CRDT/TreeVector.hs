
module CRDT.TreeVector (
  TreeVector,
  getVector,
  Client(..),
  mkPatch,
) where

import           CRDT.TreeVector.Internal

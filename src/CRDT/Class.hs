{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module CRDT.Class where

import           Data.Semigroup
import           GHC.Generics

data Client
  = Client Integer
  deriving (Show, Eq, Ord, Generic)

class Semigroup crdt => CRDT crdt where
  type External crdt :: *
  query :: crdt -> External crdt
  mkPatch :: Client -> crdt -> External crdt -> crdt

update :: CRDT crdt => Client -> crdt -> External crdt -> crdt
update client old new =
  mkPatch client old new <> old

mkNew :: (Monoid crdt, CRDT crdt) => Client -> External crdt -> crdt
mkNew client new = mkPatch client mempty new

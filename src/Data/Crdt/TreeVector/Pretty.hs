{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Data.Crdt.TreeVector.Pretty (ppTree) where

import           Data.Map (toAscList, null)
import qualified Data.Tree as DT

import           Data.Crdt.TreeVector.Internal

ppTree :: Show a => TreeVector clientId a -> String
ppTree = DT.drawTree . convertTreeVector

convertTreeVector :: Show a => TreeVector clientId a -> DT.Tree String
convertTreeVector (TreeVector tree _) = DT.Node
  (if Data.Map.null tree then "{}" else "TreeVector")
  (map (convertNode . snd) (toAscList tree))

convertNode :: Show a => Node clientId a -> DT.Tree String
convertNode (Node left e right) = DT.Node
  "Node"
  [convertTreeVector left, convertElement e, convertTreeVector right]

convertElement :: Show a => Element a -> DT.Tree String
convertElement e = DT.Node (show e) []

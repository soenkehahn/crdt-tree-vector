{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Crdt.TreeVector.Internal (
  Element(..),
  get,

  Node(..),
  getNodeVector,

  Client(..),
  TreeVector(..),
  getVector,

  mkPatch,
  diff,
) where

import           Data.List
import           Data.Map (Map, toAscList, unionWith)
import qualified Data.Map as Map
import           Data.Semigroup hiding (diff)
import           Data.Typeable
import           GHC.Generics

import           Data.Crdt.TreeVector.Internal.Edit

-- * characters

data Element a
  = Set a
  | Deleted
  deriving (Show, Eq, Generic, Typeable)

instance Ord a => Semigroup (Element a) where
  Deleted <> _ = Deleted
  _ <> Deleted = Deleted
  Set a <> Set b = Set (max a b)

get :: Element a -> [a]
get = \ case
  Set c -> [c]
  Deleted -> []

-- * trees

data Client
  = Client Integer
  deriving (Show, Eq, Ord, Generic)

data Node a
  = Node (TreeVector a) (Element a) (TreeVector a)
  deriving (Show, Eq, Generic, Typeable)

instance Ord a => Semigroup (Node a) where
  (Node l1 c1 r1) <> (Node l2 c2 r2) =
    Node (l1 <> l2) (c1 <> c2) (r1 <> r2)

mkNode :: Ord a => a -> Node a
mkNode c = Node mempty (Set c) mempty

getNodeVector :: Node a -> [a]
getNodeVector (Node left c right) =
  getVector left ++ get c ++ getVector right

nodeLength :: Node a -> Int
nodeLength = length . getNodeVector

data TreeVector a
  = TreeVector (Map Client (Node a))
  deriving (Show, Eq, Generic, Typeable)

instance Ord a => Semigroup (TreeVector a) where
  TreeVector a <> TreeVector b =
    TreeVector $ unionWith (<>) a b

instance Ord a => Monoid (TreeVector a) where
  mappend = (<>)
  mempty = TreeVector mempty

getVector :: TreeVector a -> [a]
getVector (TreeVector m) =
  concatMap (getNodeVector . snd) $ toAscList m

treeLength :: TreeVector a -> Int
treeLength = length . getVector

mkPatch :: forall a . Ord a => Client -> TreeVector a -> [a] -> TreeVector a
mkPatch client tree s =
  foldl' treeAdd tree (diff (getVector tree) s)
  where
    treeAdd :: TreeVector a -> Edit a -> TreeVector a
    treeAdd (TreeVector tree) edit = TreeVector $
      Map.fromList $ mapAdd (toAscList tree) edit

    mapAdd :: [(Client, Node a)] -> Edit a -> [(Client, Node a)]
    mapAdd [] (Insert 0 c) = [(client, mkNode c)]
    mapAdd [(client, sub)] edit = [(client, nodeAdd sub edit)]
    mapAdd m edit = mapAddMult m edit

    mapAddMult :: [(Client, Node a)] -> Edit a -> [(Client, Node a)]
    mapAddMult [(client, node)] edit=
      [(client, nodeAdd node edit)]
    mapAddMult ((client, a) : r) edit
      | index edit < nodeLength a =
        ((client, nodeAdd a edit) : r)
      | index edit >= nodeLength a =
        ((client, a) : mapAddMult r (modIndex (subtract (nodeLength a)) edit))
    mapAddMult [] (Insert 0 _) = error "hole" -- fixme
    mapAddMult _ _ = error $ show "mapAddMult"

    nodeAdd :: Node a -> Edit a -> Node a
    nodeAdd (Node left c right) edit
      | index edit < treeLength left =
        Node (treeAdd left edit) c right
      | index edit == treeLength left =
        case c of
          Set _ ->
            case edit of
              Delete _ ->
                Node left Deleted right
              Insert _ _ ->
                Node (treeAdd left edit) c right
          Deleted ->
            case edit of
              Insert _ new ->
                Node left c (treeAdd right (Insert 0 new))
              Delete _ ->
                Node left c (treeAdd right (Delete 0))
      | index edit > treeLength left =
        Node left c (treeAdd right (modIndex (subtract (treeLength left + length (get c))) edit))
    nodeAdd _ _ = error $ show "nodeAdd"

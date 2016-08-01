{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Data.Crdt.TreeVector.Internal (
  Element(..),
  get,

  Node(..),
  getNodeVector,
  nodeLength,

  Client(..),
  TreeVector(..),
  mapHash,
  treeVector,
  getVector,
  getVectorWithClients,
  treeLength,

  mkPatch,
  diff,
) where

import           Control.DeepSeq
import           Data.Hashable
import           Data.List
import           Data.Map (Map, toAscList, unionWith)
import qualified Data.Map as Map
import           Data.Semigroup hiding (diff)
import           Data.Typeable
import           GHC.Generics (Generic)

import           Data.Crdt.TreeVector.Internal.Edit

-- * characters

data Element a
  = Set a
  | Deleted
  deriving (Show, Eq, Generic, Typeable, Functor)

instance NFData a => NFData (Element a)

instance Hashable a => Hashable (Element a)

instance Ord a => Semigroup (Element a) where
  Deleted <> _ = Deleted
  _ <> Deleted = Deleted
  Set a <> Set b = Set (max a b)

get :: Element a -> [a]
get = \ case
  Set c -> [c]
  Deleted -> []

-- * trees

data Client clientId
  = Client clientId
  deriving (Show, Eq, Ord, Generic)

instance NFData clientId => NFData (Client clientId)

instance Hashable clientId => Hashable (Client clientId)

data Node clientId a
  = Node (TreeVector clientId a) (Element a) (TreeVector clientId a)
  deriving (Show, Eq, Generic, Typeable, Functor)

instance (NFData clientId, NFData a) => NFData (Node clientId a)

instance (Hashable clientId, Hashable a) => Hashable (Node clientId a) where
  hashWithSalt salt (Node left e right) =
    salt `hashWithSalt`
    treeVectorHash left `hashWithSalt`
    e `hashWithSalt`
    treeVectorHash right

instance (Ord clientId, Hashable clientId, Ord a, Hashable a) =>
  Semigroup (Node clientId a) where

  (Node l1 c1 r1) <> (Node l2 c2 r2) =
    Node (l1 <> l2) (c1 <> c2) (r1 <> r2)

mkNode :: (Ord clientId, Hashable clientId, Ord a, Hashable a) =>
  a -> Node clientId a
mkNode c = Node mempty (Set c) mempty

getNodeVector :: Node clientId a -> [a]
getNodeVector (Node left c right) =
  getVector left ++ get c ++ getVector right

getNodeVectorWithClients :: Client clientId -> Node clientId a
  -> [(Client clientId, a)]
getNodeVectorWithClients client (Node left c right) =
  getVectorWithClients left ++
  fmap (client,) (get c) ++
  getVectorWithClients right

nodeLength :: Node clientId a -> Int
nodeLength = length . getNodeVector

data TreeVector clientId a
  = TreeVector {
    treeMap :: Map (Client clientId) (Node clientId a),
    treeVectorHash :: Int
  }
  deriving (Show, Eq, Generic, Typeable, Functor)

mapHash :: forall k v . (Hashable k, Hashable v) =>
  Data.Map.Map k v -> Int
mapHash m =
  (\ (a :*: b) -> hashWithSalt a b) $
  Map.foldlWithKey' inner (0 :*: 0) m
  where
    inner :: Int :*: Int -> k -> v -> Int :*: Int
    inner (salt :*: l) k v =
      (salt `hashWithSalt` k `hashWithSalt` v) :*:
      (l + 1)

data a :*: b
  = !a :*: !b

treeVector :: (Hashable clientId, Hashable a) =>
  Map (Client clientId) (Node clientId a) -> TreeVector clientId a
treeVector m = TreeVector m (mapHash m)

instance (NFData clientId, NFData a) => NFData (TreeVector clientId a)

instance (Ord clientId, Hashable clientId, Ord a, Hashable a) =>
  Semigroup (TreeVector clientId a) where

  a <> b = if treeVectorHash a == treeVectorHash b
    then a
    else treeVector $ unionWith (<>) (treeMap a) (treeMap b)

instance (Ord clientId, Hashable clientId, Ord a, Hashable a) =>
  Monoid (TreeVector clientId a) where

  mappend = (<>)
  mempty = treeVector mempty

getVector :: TreeVector clientId a -> [a]
getVector =
  fmap snd . getVectorWithClients

getVectorWithClients :: TreeVector clientId a -> [(Client clientId, a)]
getVectorWithClients (TreeVector m _) =
  concatMap (uncurry getNodeVectorWithClients) $ toAscList m

treeLength :: TreeVector clientId a -> Int
treeLength = length . getVector

mkPatch :: forall clientId a . (Ord clientId, Hashable clientId, Ord a, Hashable a) =>
  Client clientId -> TreeVector clientId a -> [a] -> TreeVector clientId a
mkPatch client tree s =
  foldl' treeAdd tree (diff (getVector tree) s)
  where
    treeAdd :: TreeVector clientId a -> Edit a -> TreeVector clientId a
    treeAdd (TreeVector tree _) edit = treeVector $
      Map.fromList $ mapAdd (toAscList tree) edit

    mapAdd :: [(Client clientId, Node clientId a)] -> Edit a
      -> [(Client clientId, Node clientId a)]
    mapAdd [] (Insert 0 c) = [(client, mkNode c)]
    mapAdd [(client, sub)] edit = [(client, nodeAdd sub edit)]
    mapAdd m edit = mapAddMult m edit

    mapAddMult :: [(Client clientId, Node clientId a)] -> Edit a
      -> [(Client clientId, Node clientId a)]
    mapAddMult [(client, node)] edit=
      [(client, nodeAdd node edit)]
    mapAddMult ((client, a) : r) edit
      | index edit < nodeLength a =
        ((client, nodeAdd a edit) : r)
      | index edit >= nodeLength a =
        ((client, a) : mapAddMult r (modIndex (subtract (nodeLength a)) edit))
    mapAddMult [] (Insert 0 _) = error "hole" -- fixme
    mapAddMult _ _ = error $ show "mapAddMult"

    nodeAdd :: Node clientId a -> Edit a -> Node clientId a
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

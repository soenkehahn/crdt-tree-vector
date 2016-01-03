{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module CRDT.TreeVector.Internal (
  Element(..),
  get,

  Node(..),
  getNodeDoc,

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

import           CRDT.TreeVector.Internal.Edit

-- * characters

data Element
  = Set Char
  | Deleted
  deriving (Show, Eq, Generic, Typeable)

instance Semigroup Element where
  Deleted <> _ = Deleted
  _ <> Deleted = Deleted
  Set a <> Set b = Set (max a b)

get :: Element -> String
get = \ case
  Set c -> [c]
  Deleted -> ""

-- * trees

data Client
  = Client Integer
  deriving (Show, Eq, Ord, Generic)

data Node
  = Node TreeVector Element TreeVector
  deriving (Show, Eq, Generic, Typeable)

instance Semigroup Node where
  (Node l1 c1 r1) <> (Node l2 c2 r2) =
    Node (l1 <> l2) (c1 <> c2) (r1 <> r2)

mkNode :: Char -> Node
mkNode c = Node mempty (Set c) mempty

getNodeDoc :: Node -> String
getNodeDoc (Node left c right) =
  getVector left ++ get c ++ getVector right

nodeLength :: Node -> Int
nodeLength = length . getNodeDoc

data TreeVector
  = TreeVector (Map Client Node)
  deriving (Show, Eq, Generic, Typeable)

instance Semigroup TreeVector where
  TreeVector a <> TreeVector b =
    TreeVector $ unionWith (<>) a b

instance Monoid TreeVector where
  mappend = (<>)
  mempty = TreeVector mempty

getVector :: TreeVector -> String
getVector (TreeVector m) =
  concatMap (getNodeDoc . snd) $ toAscList m

treeLength :: TreeVector -> Int
treeLength = length . getVector

mkPatch :: Client -> TreeVector -> String -> TreeVector
mkPatch client tree s =
  foldl' treeAdd tree (diff (getVector tree) s)
  where
    treeAdd :: TreeVector -> Edit -> TreeVector
    treeAdd (TreeVector tree) edit = TreeVector $
      Map.fromList $ mapAdd (toAscList tree) edit

    mapAdd [] (Insert 0 c) = [(client, mkNode c)]
    mapAdd [(client, sub)] edit = [(client, nodeAdd sub edit)]
    mapAdd m edit = mapAddMult m edit

    mapAddMult :: [(Client, Node)] -> Edit -> [(Client, Node)]
    mapAddMult [(client, node)] edit=
      [(client, nodeAdd node edit)]
    mapAddMult ((client, a) : r) edit
      | index edit < nodeLength a =
        ((client, nodeAdd a edit) : r)
      | index edit >= nodeLength a =
        ((client, a) : mapAddMult r (modIndex (subtract (nodeLength a)) edit))
    mapAddMult [] (Insert 0 _) = error "hole" -- fixme
    mapAddMult m x = error $ show ("mapAddMult", m, x)

    nodeAdd :: Node -> Edit -> Node
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
    nodeAdd n e = error $ show ("nodeAdd", n, e)

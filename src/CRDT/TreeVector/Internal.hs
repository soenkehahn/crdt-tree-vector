{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module CRDT.TreeVector.Internal (
  DChar(..),
  get,

  Node(..),
  getNodeDoc,

  Client(..),
  Tree(..),
  getDocument,

  update,
  diff,
) where

import           Data.List
import           Data.Map (Map, toAscList, unionWith)
import qualified Data.Map as Map
import qualified Data.Patch as P
import           Data.Semigroup hiding (diff)
import qualified Data.Vector as V
import           GHC.Generics

-- * characters

data DChar
  = DChar Char
  | Deleted
  deriving (Show, Eq, Generic)

instance Semigroup DChar where
  Deleted <> _ = Deleted
  _ <> Deleted = Deleted
  DChar a <> DChar b = DChar (max a b)

get :: DChar -> String
get = \ case
  DChar c -> [c]
  Deleted -> ""

-- * trees

data Client
  = Client Integer
  deriving (Show, Eq, Ord, Generic)

data Node
  = Node Tree DChar Tree
  deriving (Show, Eq, Generic)

instance Semigroup Node where
  (Node l1 c1 r1) <> (Node l2 c2 r2) =
    Node (l1 <> l2) (c1 <> c2) (r1 <> r2)

mkNode :: Char -> Node
mkNode c = Node mempty (DChar c) mempty

getNodeDoc :: Node -> String
getNodeDoc (Node left c right) =
  getDocument left ++ get c ++ getDocument right

nodeLength = length . getNodeDoc

data Tree
  = Tree (Map Client Node)
  deriving (Show, Eq, Generic)

instance Semigroup Tree where
  Tree a <> Tree b =
    Tree $ unionWith (<>) a b

instance Monoid Tree where
  mappend = (<>)
  mempty = Tree mempty

getDocument :: Tree -> String
getDocument (Tree m) =
  concatMap (getNodeDoc . snd) $ toAscList m

treeLength = length . getDocument

update :: Client -> Tree -> String -> Tree
update client tree s =
  foldl' treeAdd tree (diff tree s)
  where
    treeAdd :: Tree -> P.Edit Char -> Tree
    treeAdd (Tree tree) edit = Tree $ Map.fromList $ mapAdd (toAscList tree) edit

    mapAdd [] (P.Insert 0 c) = [(client, mkNode c)]
    mapAdd [(client, sub)] edit = [(client, nodeAdd sub edit)]
    mapAdd m edit = mapAddMult m edit

    mapAddMult :: [(Client, Node)] -> P.Edit Char -> [(Client, Node)]
    mapAddMult [(client, node)] edit=
      [(client, nodeAdd node edit)]
    mapAddMult ((client, a) : r) edit
      | index edit < nodeLength a =
        ((client, nodeAdd a edit) : r)
      | index edit >= nodeLength a =
        ((client, a) : mapAddMult r (modIndex (subtract (nodeLength a)) edit))
    mapAddMult [] (P.Insert 0 c) = error "hole" -- fixme
    mapAddMult m x = error $ show ("mapAddMult", m, x)

    nodeAdd :: Node -> P.Edit Char -> Node
    nodeAdd (Node left c right) edit
      | index edit < treeLength left =
        Node (treeAdd left edit) c right
      | index edit == treeLength left =
        case c of
          DChar _ ->
            case edit of
              P.Delete _ _ -> Node left Deleted right
              P.Insert _ new ->
                Node (treeAdd left edit) c right
              x -> error $ show ("nodeAdd", x)
          Deleted ->
            case edit of
              P.Insert _ new ->
                Node left c (treeAdd right (P.Insert 0 new))
              P.Delete _ old ->
                Node left c (treeAdd right (P.Delete 0 old))
      | index edit > treeLength left =
        Node left c (treeAdd right (modIndex (subtract (treeLength left + length (get c))) edit))
    nodeAdd n e = error $ show ("nodeAdd", n, e)

-- * patches

diff :: Tree -> String -> [P.Edit Char]
diff tree s =
  updateIndices 0 $ P.toList $
  P.diff (V.fromList (getDocument tree)) (V.fromList s)
  where
    updateIndices offset (P.Insert i c : r) =
      P.Insert (offset + i) c : updateIndices (succ offset) r
    updateIndices offset (P.Delete i old : r) =
      P.Delete (offset + i) old : updateIndices (pred offset) r
    updateIndices offset (P.Replace i old new : r) =
      P.Delete (offset + i) old : P.Insert (offset + i) new : updateIndices offset r
    updateIndices _ [] = []

modIndex :: (Int -> Int) -> P.Edit a -> P.Edit a
modIndex f = \ case
  P.Insert i c -> P.Insert (f i) c
  P.Delete i c -> P.Delete (f i) c

index :: P.Edit a -> Int
index = \ case
  P.Insert i _ -> i
  P.Delete i _ -> i
  P.Replace i _ _ -> i

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Crdt.TreeVector.Cursor (
  Cursor,
  toCursor,
  fromCursor,
) where

import           Data.Map hiding (map)

import           Data.Crdt.TreeVector.Internal hiding (get)

type Cursor = Maybe MapCursor

data MapCursor
  = Descend Client NodeCursor
  deriving (Show, Eq)

data NodeCursor
  = NodeLeft MapCursor
  | NodeElement ElementCursor
  | NodeRight MapCursor
  deriving (Show, Eq)

type ElementCursor = ()

data CursorResult a
  = Here a
  | Later Int
  deriving (Show, Eq, Functor)

data CursorResultM a
  = CursorResultM (Int -> CursorResult a)
  deriving (Functor)

class ToCursor a cursor where
  to :: a -> CursorResultM cursor

(>>>) :: CursorResultM a -> CursorResultM a -> CursorResultM a
infixr >>>
CursorResultM a >>> CursorResultM b = CursorResultM $ \ index -> case a index of
  Here a -> Here a
  Later i -> case b (index - i) of
    Here result -> Here result
    Later j -> Later (i + j)

toCursor :: Ord a => TreeVector a -> Int -> Cursor
toCursor tree index =
  let CursorResultM f = to tree
  in case f index of
    Here result -> Just result
    Later _ -> Nothing

instance ToCursor (TreeVector a) MapCursor where
  to = to . treeMap

instance ToCursor (Map Client (Node a)) MapCursor where
  to = to . toAscList

instance ToCursor [(Client, Node a)] MapCursor where
  to = \ case
    [] -> CursorResultM $ \ _ -> Later 0
    (client, node) : r ->
      (Descend client <$> to node) >>> to r

instance ToCursor (Node a) NodeCursor where
  to (Node left e right) =
    (NodeLeft <$> to left) >>>
    (NodeElement <$> to e) >>>
    (NodeRight <$> to right)

instance ToCursor (Element a) ElementCursor where
  to element = CursorResultM $ \ index ->
    case (element, index) of
      (Set _, 0) -> Here ()
      (Set _, _) -> Later 1
      (Deleted, _) -> Later 0

fromCursor :: TreeVector a -> Cursor -> Int
fromCursor tree cursor = case cursor of
  Just c -> from tree c
  Nothing -> treeLength tree

class FromCursor a cursor where
  from :: a -> cursor -> Int

instance FromCursor (TreeVector a) MapCursor where
  from (TreeVector tree) cursor = from tree cursor

instance FromCursor (Map Client (Node a)) MapCursor where
  from m = from (toAscList m)

instance FromCursor [(Client, Node a)] MapCursor where
  from children cursor = case cursor of
    (Descend client next) -> case children of
      (c, n) : _ | c == client -> from n next
      (_, n) : r -> nodeLength n + from r (Descend client next)
      [] -> 0 -- shouldn't happen with a correct tree

instance FromCursor (Node a) NodeCursor where
  from (Node l e r) cursor = case cursor of
    NodeLeft next -> from (treeMap l) next
    NodeElement next -> treeLength l + from e next
    NodeRight next -> treeLength l + elementLength e + from (treeMap r) next

instance FromCursor (Element a) ElementCursor where
  from element cursor = case (element, cursor) of
    (Set _, ()) -> 0
    (Deleted, ()) -> 0

elementLength :: Element a -> Int
elementLength = \ case
  Set _ -> 1
  Deleted -> 0

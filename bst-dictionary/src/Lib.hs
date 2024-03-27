module Lib where

import Prelude hiding (lookup)


data BST key item = Node key item (BST key item) (BST key item) | Empty
    deriving (Eq, Show)


emptyTree :: BST key item
emptyTree = Empty

insert :: Ord key => key -> item -> BST key item -> BST key item
insert newKey newItem Empty = Node newKey newItem Empty Empty
insert newKey newItem (Node key item leftChild rightChild) 
    | newKey < key = Node key item (insert newKey newItem leftChild) rightChild
    | newKey > key = Node key item leftChild (insert newKey newItem rightChild)
    | newKey == key = Node key newItem leftChild rightChild

lookup :: Ord key => key -> BST key item -> Maybe item
lookup soughtKey Empty = Nothing
lookup soughtKey (Node key item leftChild rightChild)
    | soughtKey < key  = lookup soughtKey leftChild
    | soughtKey > key  = lookup soughtKey rightChild
    | soughtKey == key = Just item

list :: BST key item -> [(key, item)]
list Empty = []
list (Node key item leftChild rightChild) = list leftChild ++ [(key, item)] ++ list rightChild


remove :: (Ord key, Eq item) => key -> BST key item -> BST key item
remove _ Empty = Empty
remove removeKey (Node key item leftChild rightChild)
    | removeKey < key  = Node key item (remove removeKey leftChild) rightChild
    | removeKey > key  = Node key item leftChild (remove removeKey rightChild)
    | removeKey == key = case (leftChild, rightChild) of
                              (Empty,     _)     -> rightChild
                              (_,         Empty) -> leftChild
                              (_,         _)     -> let replacementNode = findMinimumNode leftChild
                                                    in Node (fst replacementNode) (snd replacementNode) (remove (fst replacementNode) leftChild) rightChild

findMinimumNode :: BST key item -> (key, item)
findMinimumNode (Node key item _ Empty) = (key, item)
findMinimumNode (Node _ _ _ rightChild) = findMinimumNode rightChild


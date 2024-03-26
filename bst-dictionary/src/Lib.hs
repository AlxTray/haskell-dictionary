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
list (Node key item leftChild rightChild) = [(key, item)]

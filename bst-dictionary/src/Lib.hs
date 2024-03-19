module Lib where


data BST key item = Node key item (BST key item) (BST key item) | Empty
    deriving (Eq, Show)


emptyTree :: BST key item
emptyTree = Empty

insert :: Ord key => key -> item -> BST key item -> BST key item
insert newKey newItem Empty = Node newKey newItem Empty Empty
insert newKey newItem (Node key item leftChild rightChild) = 
    if newKey < key
      then Node key item (insert newKey newItem leftChild) rightChild
    else if newKey > key
      then Node key item leftChild (insert newKey newItem rightChild)
    else emptyTree
insert _      _       _     = emptyTree

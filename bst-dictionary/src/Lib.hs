module Lib where


data BST key item = Node key item (BST key item) (BST key item) | Empty
    deriving (Eq, Show)


emptyTree :: BST key item
emptyTree = Empty

insert :: key -> item -> BST key item -> BST key item
insert newKey newItem Empty = Node newKey newItem Empty Empty

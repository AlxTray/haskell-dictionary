module Lib where


data BST = Node Int String (BST) (BST) | Empty
    deriving (Eq, Show)


emptyTree :: BST
emptyTree = Empty

insert :: Int -> String -> BST -> BST
insert newKey newItem oldBST = emptyTree

module Dictionary where

import Prelude hiding (lookup)
import BST


data Dictionary key item = Dictionary (BST key item)
    deriving (Eq, Show)


newDict :: Dictionary key item
newDict = Dictionary emptyTree

insertDict :: Ord key => key -> item -> Dictionary key item -> Dictionary key item
insertDict key item (Dictionary dict) = Dictionary (insert key item dict)

lookupDict :: Ord key => key -> Dictionary key item -> Maybe item
lookupDict key (Dictionary dict) = lookup key dict

listDict :: Ord key => Dictionary key item -> [(key, item)]
listDict (Dictionary dict) = list dict

removeDict :: (Ord key, Eq item) => key -> Dictionary key item -> Dictionary key item
removeDict removeKey (Dictionary dict) = Dictionary (remove removeKey dict)

removeAllDict :: (Ord key, Eq item) => (key -> Bool) -> Dictionary key item -> Dictionary key item
removeAllDict predicate (Dictionary dict) = Dictionary (removeAll predicate dict)

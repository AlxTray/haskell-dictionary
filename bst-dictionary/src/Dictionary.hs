module Dictionary where

import BST

data Dictionary key item = Dictionary (BST key item)


newDict :: Dictionary key item
newDict = emptyTree

insertDict :: key -> item -> Dictionary key item -> Dictionary key item
insertDict key item (Dictionary dict) = Dictionary (insert key item dict)

lookupDict :: key -> Dictionary key item -> Maybe item
lookupDict key (Dictionary dict) = lookup key dict

listDict :: Dictionary key item -> [(key, item)]
listDict (Dictionary dict) = list dict

removeDict :: key -> Dictionary key item -> Dictionary key item
removeDict removeKey (Dictionary dict) = remove removeKey dict

removeAllDict :: (key -> Bool) -> Dictionary key item -> Dictionary key item
removeAllDict predicate (Dictionary dict) = removeAll predicate dict

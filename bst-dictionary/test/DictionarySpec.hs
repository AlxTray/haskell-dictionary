module DictionarySpec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Maybe
import qualified Data.Set as Set

import Dictionary
import BST


instance (Arbitrary key, Arbitrary item, Ord key) => Arbitrary (BST key item) where
    -- This is done so an empty set for the keys to be stored can be passed
    arbitrary = sized (\numberOfNodes -> generateSubTree numberOfNodes Set.empty)

generateSubTree :: (Arbitrary key, Arbitrary item, Ord key) => Int -> Set.Set key -> Gen (BST key item)
generateSubTree 0 _ = return Empty
generateSubTree numberOfNodes keysInTree = do
    -- Use of set to only allow unique keys idea adapted from question at
    -- https://stackoverflow.com/questions/35528777/quickcheck-produce-arbitrary-elements-of-an-arbitrary-set
    key <- arbitrary `suchThat` (\randomKey -> not (Set.member randomKey keysInTree))
    item <- arbitrary
    leftSize <- choose (0, numberOfNodes - 1)
    rightSize <- choose (0, numberOfNodes - 1)

    let keysInTreeWithNew = Set.insert key keysInTree
    left <- generateSubTree leftSize keysInTreeWithNew
    right <- generateSubTree rightSize keysInTreeWithNew

    return $ Node key item left right


instance (Arbitrary key, Arbitrary item, Ord key) => Arbitrary (Dictionary key item) where
    arbitrary = do
        tree <- arbitrary
        return $ Dictionary tree


prop_insertThenLookup :: Int -> Int -> Dictionary Int Int -> Property
prop_insertThenLookup key item dict =
    property (lookupDict key (insertDict key item dict) == Just item)

prop_removeAllThenList :: Dictionary Int Int -> Property
prop_removeAllThenList dict =
    property (null (listDict (removeAllDict (const True) dict)))

prop_removeThenLookup :: Dictionary Int Int -> Property
prop_removeThenLookup dict =
    property (lookupDict removeKey (removeDict removeKey dict) === Nothing)
    where
        -- Gets a particular value from the tree
        -- Otherwise a random key may not be in the random tree
        (removeKey, _) = head (listDict dict)

prop_emptyThenList :: Property
prop_emptyThenList =
    let emptyTree :: Dictionary Int Int
        emptyTree = newDict
    in
        property (null (listDict emptyTree))


allDictionaryTests :: TestTree
allDictionaryTests = testGroup "All tests for the Dictionary module (Property-based)"
    [ testProperty "A lookup after an insertion should return the inserted item" prop_insertThenLookup,
      testProperty "Listing after removing all nodes in the tree should be empty" prop_removeAllThenList,
      testProperty "Removing then attempting to lookup the removed value should return Nothing" prop_removeThenLookup,
      testProperty "Listing a new dictionary should return an empty list" prop_emptyThenList
    ]

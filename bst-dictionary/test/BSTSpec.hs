module BSTSpec where

import Test.Tasty
import Test.Tasty.HUnit
import BST

import Prelude hiding (lookup)
import Data.Maybe


testEmptyInsert :: Assertion
testEmptyInsert = do
    let tree = insert 1 "Alex" emptyTree
    assertEqual "" tree (Node 1 "Alex" Empty Empty)

testInsertOnLeft :: Assertion
testInsertOnLeft = do
    let tree = insert 3 "Jeff" (insert 5 "Dave" emptyTree)
    let expectedTree = (Node 5 "Dave" (Node 3 "Jeff" Empty Empty) Empty)
    assertEqual "" tree expectedTree

testInsertOnRight :: Assertion
testInsertOnRight = do
    let tree = insert 10 "Jeff" (insert 5 "Dave" emptyTree)
    let expectedTree = (Node 5 "Dave" Empty (Node 10 "Jeff" Empty Empty))
    assertEqual "" tree expectedTree

testInsertEqual :: Assertion
testInsertEqual = do
    let tree = insert 1 "Dave" (insert 1 "Jeff" emptyTree)
    let expectedTree = (Node 1 "Dave" Empty Empty)
    assertEqual "" tree expectedTree

insertTests :: TestTree
insertTests = testGroup "HUnit tests for 'insert'"
    [testCase "Test to see insert on empty tree has single node" testEmptyInsert,
     testCase "Test to see insert with a key less than parent is inserted on left" testInsertOnLeft,
     testCase "Test to see insert with a key more than parent is inserted on right" testInsertOnRight, 
     testCase "Test to see insert with a key equal must overwrite item of equivalent node" testInsertEqual
    ]


testEmptyLookup :: Assertion
testEmptyLookup = do
    let tree = emptyTree
    let lookupResult = lookup 1 tree
    assertBool "" (isNothing lookupResult)

testLookupKeyExists :: Assertion
testLookupKeyExists = do
    let tree = insert 1 "Alex" (insert 3 "Jeff" emptyTree)
    let lookupResult = lookup 1 tree
    assertEqual "" lookupResult (Just "Alex")

lookupTests :: TestTree
lookupTests = testGroup "HUnit tests for 'lookup'"
    [testCase "Test to see that lookup on empty tree returns nothing" testEmptyLookup,
     testCase "Test to see that lookup with key existing returns item" testLookupKeyExists
    ]


testEmptyList :: Assertion
testEmptyList = do
    let tree = emptyTree
    let returnedList = list tree
    assertBool "" (null returnedList)

testListSingleNode :: Assertion
testListSingleNode = do
    let tree = Node 1 "Dave" Empty Empty
    let expectedList = [(1, "Dave")]
    assertEqual "" (list tree) expectedList

testListManyNodes :: Assertion
testListManyNodes = do
    let tree = Node 10 "Dave" (Node 5 "Jeff" Empty Empty) (Node 15 "Alex" Empty Empty)
    let expectedList = [(5, "Jeff"), (10, "Dave"), (15, "Alex")]
    assertEqual "" (list tree) expectedList

listTests :: TestTree
listTests = testGroup "HUnit tests for 'list'"
    [testCase "Test to see that listing an empty tree returns an empty list" testEmptyList,
     testCase "Test to see listing a tree with single node returns list of single pair" testListSingleNode,
     testCase "Test to see listing a tree with many nodes returns list of all nodes in order" testListManyNodes
    ]


testEmptyRemove :: Assertion
testEmptyRemove = do
    let tree = emptyTree
    assertEqual "" (remove 1 tree) (Empty :: BST Int String)

testRemoveSingleRoot :: Assertion
testRemoveSingleRoot = do
    let tree = Node 1 "Dave" Empty Empty
    assertEqual "" (remove 1 tree) Empty

testRemoveSingleChild :: Assertion
testRemoveSingleChild = do
    let tree = Node 10 "Dave" (Node 5 "Jeff" (Node 3 "Steve" Empty Empty) Empty) (Node 15 "Alex" Empty Empty)
    let expectedTree = Node 10 "Dave" (Node 3 "Steve" Empty Empty) (Node 15 "Alex" Empty Empty)
    assertEqual "" (remove 5 tree) expectedTree

testRemoveBothChildren :: Assertion
testRemoveBothChildren = do
    let tree = Node 10 "Dave" (Node 6 "Jeff" (Node 3 "Steve" (Node 2 "Andrew" Empty Empty) (Node 4 "James" Empty (Node 5 "Ben" Empty Empty))) (Node 8 "Sam" Empty Empty)) (Node 15 "Alex" Empty Empty)
    let expectedTree = Node 10 "Dave" (Node 5 "Ben" (Node 3 "Steve" (Node 2 "Andrew" Empty Empty) (Node 4 "James" Empty Empty)) (Node 8 "Sam" Empty Empty)) (Node 15 "Alex" Empty Empty)
    assertEqual "" (remove 6 tree) expectedTree

removeTests :: TestTree
removeTests = testGroup "HUnit tests for 'remove'"
    [testCase "Test to see attempt to remove on empty tree returns empty tree" testEmptyRemove,
     testCase "Test to see if removing tree with only root returns an empty tree" testRemoveSingleRoot,
     testCase "Test to see removing node with single child gets replaced by that child" testRemoveSingleChild,
     testCase "Test to see removing node with both children should get replaced by in-order predecessor" testRemoveBothChildren
    ]


testEmptyPredicateRemove :: Assertion
testEmptyPredicateRemove = do
    let tree = emptyTree
    assertEqual "Test to see that removing by predicate on an empty tree returns an empty tree" (removeAll (>0) tree) (Empty :: BST Int String)

testPredicateRemoveAllMatch :: Assertion
testPredicateRemoveAllMatch = do
    let tree = Node 10 "Dave" (Node 6 "Jeff" (Node 3 "Steve" (Node 2 "Andrew" Empty Empty) (Node 4 "James" Empty (Node 5 "Ben" Empty Empty))) (Node 8 "Sam" Empty Empty)) (Node 15 "Alex" Empty Empty)
    assertEqual "Test to see that an empty tree is returned when all keys match predicate" (removeAll (<20) tree) (Empty :: BST Int String)

testPredicateRemoveSomeMatch :: Assertion
testPredicateRemoveSomeMatch = do
    let tree = Node 10 "Dave" (Node 6 "Jeff" (Node 3 "Steve" (Node 2 "Andrew" Empty Empty) (Node 4 "James" Empty (Node 5 "Ben" Empty Empty))) (Node 8 "Sam" Empty Empty)) (Node 15 "Alex" Empty Empty)
    let expectedTree = Node 5 "Ben" (Node 3 "Steve" Empty Empty) (Node 15 "Alex" Empty Empty)
    assertEqual "Test to see that tree contains only nodes that were not removed" (removeAll (\x -> x `mod` 2 == 0) tree) expectedTree

predicateRemoveTests :: TestTree
predicateRemoveTests = testGroup "HUnit tests for 'removeAll'"
    [testCase "Test to see that removing by predicate on an empty tree returns an empty tree" testEmptyPredicateRemove,
     testCase "Test to see that an empty tree is returned when all keys match predicate" testPredicateRemoveAllMatch,
     testCase "Test to see that tree contains only nodes that were not removed" testPredicateRemoveSomeMatch
    ]



allBSTTests :: TestTree
allBSTTests = testGroup "All tests for the BST module"
    [insertTests, lookupTests, listTests, removeTests, predicateRemoveTests]

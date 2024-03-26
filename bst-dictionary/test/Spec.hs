import Test.HUnit
import Lib

import Prelude hiding (lookup)
import Data.Maybe


testEmptyInsert :: Test
testEmptyInsert = TestCase $ do
    let tree = insert 1 "Alex" emptyTree
    assertEqual "Test to see insert on empty tree has single node" tree (Node 1 "Alex" Empty Empty)

testInsertOnLeft :: Test
testInsertOnLeft = TestCase $ do
    let tree = insert 3 "Jeff" (insert 5 "Dave" emptyTree)
    let expectedTree = (Node 5 "Dave" (Node 3 "Jeff" Empty Empty) Empty)
    assertEqual "Test to see insert with a key less than parent is inserted on left" tree expectedTree

testInsertOnRight :: Test
testInsertOnRight = TestCase $ do
    let tree = insert 10 "Jeff" (insert 5 "Dave" emptyTree)
    let expectedTree = (Node 5 "Dave" Empty (Node 10 "Jeff" Empty Empty))
    assertEqual "Test to see insert with a key more than parent is inserted on right" tree expectedTree

testInsertEqual :: Test
testInsertEqual = TestCase $ do
    let tree = insert 1 "Dave" (insert 1 "Jeff" emptyTree)
    let expectedTree = (Node 1 "Dave" Empty Empty)
    assertEqual "Test to see insert with a key equal must overwrite item of equivalent node" tree expectedTree


testEmptyLookup :: Test
testEmptyLookup = TestCase $ do
    let tree = emptyTree
    let lookupResult = lookup 1 tree
    assertBool "Test to see that lookup on empty tree returns nothing" (isNothing lookupResult)

testLookupKeyExists :: Test
testLookupKeyExists = TestCase $ do
    let tree = insert 1 "Alex" (insert 3 "Jeff" emptyTree)
    let lookupResult = lookup 1 tree
    assertEqual "Test to see that lookup with key existing returns item" lookupResult (Just "Alex")


testEmptyList :: Test
testEmptyList = TestCase $ do
    let tree = emptyTree
    let returnedList = list tree
    assertBool "Test to see that listing an empty tree returns an empty list" (null returnedList)

testListSingleNode :: Test
testListSingleNode = TestCase $ do
    let tree = Node 1 "Dave" Empty Empty
    let expectedList = [(1, "Dave")]
    assertEqual "Test to see listing a tree with single node returns list of single pair" (list tree) expectedList

testListManyNodes :: Test
testListManyNodes = TestCase $ do
    let tree = Node 10 "Dave" (Node 5 "Jeff" Empty Empty) (Node 15 "Alex" Empty Empty)
    let expectedList = [(5, "Jeff"), (10, "Dave"), (15, "Alex")]
    assertEqual "Test to see listing a tree with many nodes returns list of all nodes in order" (list tree) expectedList


testEmptyRemove :: Test
testEmptyRemove = TestCase $ do
    let tree = emptyTree
    assertEqual "Test to see attempt to remove on empty tree returns empty tree" (remove 1 tree) (Empty :: BST Int String)

testRemoveSingleRoot :: Test
testRemoveSingleRoot = TestCase $ do
    let tree = Node 1 "Dave" Empty Empty
    assertEqual "Test to see if removing tree with only root returns an empty tree" (remove 1 tree) Empty


allTests :: Test
allTests = TestList [
    testEmptyInsert,
    testInsertOnLeft,
    testInsertOnRight,
    testInsertEqual,

    testEmptyLookup,
    testLookupKeyExists,

    testEmptyList,
    testListSingleNode,
    testListManyNodes,

    testEmptyRemove,
    testRemoveSingleRoot
 ]


main = do runTestTT allTests

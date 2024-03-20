import Test.HUnit
import Lib

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


allTests :: Test
allTests = TestList [
    testEmptyInsert,
    testInsertOnLeft,
    testInsertOnRight,
    testInsertEqual
 ]


main = do runTestTT allTests

import Test.HUnit
import Lib

testEmptyInsert :: Test
testEmptyInsert = TestCase $ do
    let tree = insert 1 "Alex" emptyTree
    assertEqual "Test to see insert on empty tree has single node" tree (Node 1 "Alex" Empty Empty)


allTests :: Test
allTests = TestList [
    testEmptyInsert
 ]


main = do runTestTT allTests

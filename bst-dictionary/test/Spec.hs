import Test.Tasty

import BSTSpec
import DictionarySpec

allTests :: TestTree
allTests = testGroup "All tests"
    [BSTSpec.allBSTTests, DictionarySpec.allDictionaryTests]


main :: IO ()
main = defaultMain allTests

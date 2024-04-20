module DictionarySpec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Dictionary

propertyNewDictionaryEmpty Dictionary key item -> Property
propertyNewDictionaryEmpty = 

allDictionaryTests :: TestTree
allDictionaryTests = testGroup "All tests for the Dictionary module"
    []

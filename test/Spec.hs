import Test.HUnit

import Day24Test


tests :: Test.HUnit.Test
tests = TestList [
   TestLabel "test2" testScanBoxID
  ,TestLabel "test1" testCharDiff]


main :: IO Counts
main =  runTestTT tests
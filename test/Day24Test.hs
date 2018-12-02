module Day24Test where

import Test.HUnit

import Day24


testScanBoxID :: Test
testScanBoxID = TestCase $ assertEqual "The scanBoxID function works as expected."
                                       [(0, 0), (1, 1), (0, 1)]
                                       (map scanBoxID ["abcdef", "bababc", "ababab"])


testCharDiff :: Test
testCharDiff = TestCase $ assertEqual "The charDiff function works as expected."
                                      [2, 1]
                                      [charDiff "abcde" "axcye", charDiff "fghij" "fguij"]

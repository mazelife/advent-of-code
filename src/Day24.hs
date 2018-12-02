{-# LANGUAGE ViewPatterns #-}
module Day24 where

import Prelude hiding (readFile, putStrLn)
import Paths_advent_of_code

import           Data.List
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust, catMaybes)
import qualified Data.Text as T
import           Data.Text.IO (readFile, putStrLn)


type BoxID = T.Text
type FrequencyDistribution = Map.Map Char Int


-- | Read a file with a list of Box IDs, one per line.
readBoxIDData :: FilePath -> IO [BoxID]
readBoxIDData = fmap (map (T.strip) . T.lines) . readFile


charFreqDist :: FrequencyDistribution -> BoxID -> FrequencyDistribution
charFreqDist fd (T.uncons -> Nothing) = fd
charFreqDist fd (T.uncons -> Just (char, remainder)) = charFreqDist newFD remainder
  where newFD = Map.insertWith (+) char 1 fd
charFreqDist _ _ = Map.empty


charWithFreqExists :: FrequencyDistribution -> Int -> Int
charWithFreqExists fd count = if isJust (find (==count) (Map.elems fd)) then 1 else 0

filterFreqDist :: FrequencyDistribution -> (Int, Int)
filterFreqDist fd = (charWithFreqExists fd 2, charWithFreqExists fd 3)


scanBoxID :: BoxID -> (Int, Int)
scanBoxID = filterFreqDist . charFreqDist Map.empty

sumCount :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumCount (a, b) (x, y) = (a + x, b + y)


charDiff :: BoxID -> BoxID -> Int
charDiff left right = mismatchedPairs $ T.zip left right
  where mismatchedPairs :: Eq a => [(a, a)] -> Int
        mismatchedPairs = length . filter (uncurry (/=))

sharedChars :: BoxID -> BoxID -> BoxID
sharedChars left right = _shared $ T.zip left right
  where _shared = T.pack . catMaybes . map (\(a, b) -> if a == b then Just a else Nothing)


pairwisePermutations :: [a] -> [(a, a)]
pairwisePermutations [] = []
pairwisePermutations [_] = []
pairwisePermutations (x:xs) = (permute (x, xs)) ++ pairwisePermutations xs
  where permute :: (a, [a]) -> [(a, a)]
        permute = uncurry (map . (,))


findBoxIDs :: [BoxID] -> Maybe (Int, BoxID, BoxID)
findBoxIDs ids = find (\(d, _, _) -> d == targetLength) checkedPairs
  where allPairs = pairwisePermutations ids
        targetLength = 1
        checkedPairs = [(charDiff x y, x, y) | (x, y) <- allPairs]


solveDay :: IO ()
solveDay = do
  pth <- getDataFileName "data/day24.txt"
  ids <- readBoxIDData pth
  let counts = map scanBoxID ids
  let (twoCounts, threeCounts) = foldl1 sumCount counts
  putStrLn $ T.pack ("Checksum: " ++ (show (threeCounts * twoCounts)))
  let boxId = case findBoxIDs ids of Just (_, a, b) -> sharedChars a b
                                     Nothing -> "No match found."
  putStrLn (T.concat ["Box ID we want: ", boxId])


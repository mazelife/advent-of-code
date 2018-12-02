module Day25 (solveDay) where

import Prelude hiding (readFile, readList)
import Paths_advent_of_code

import           Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.IO (readFile)
import           Data.Text.Read (decimal, signed)


-- | A frequency is an integer, and also a monoid, where the binary operator
-- is addition and the identity element is zero.
type Frequency = Sum Int

mkFrequency :: Int -> Frequency
mkFrequency = Sum

-- | Read a file with a list of frequencies, one per line.
readList :: FilePath -> IO [Frequency]
readList = fmap (map (parseInt . T.strip) . T.lines) . readFile

-- | Convert a textual representation (e.g. +5, -30) to a Frequency.
parseInt:: T.Text -> Frequency
parseInt txt = case signed decimal txt of
  Left _ -> mempty
  Right (i, _) -> mkFrequency i

-- | Find the first time something is repeated in a list of things.
findFirstRepetition :: Ord a => Set.Set a -> [a] -> Maybe a
findFirstRepetition _ [] = Nothing
findFirstRepetition seen (f:fs) = if f `Set.member` seen then Just f else findFirstRepetition  (f `Set.insert` seen) fs

-- | Given an initial frequency and a sequence of changes, apply each change
-- to the original, and see when a result of this is repeated for the first time.
-- If we exhaust the list of changes, start again from the beginning.
findFirstFrequencyRepetition :: Frequency -> [Frequency] -> Maybe Frequency
findFirstFrequencyRepetition initial fs = findFirstRepetition Set.empty fStream
  where fStream = scanl (+) initial (cycle fs)

solveDay :: IO ()
solveDay = do
  pth <- getDataFileName "data/day25.txt"
  fs <- readList pth
  let solution  = findFirstFrequencyRepetition mempty fs
  putStrLn (case solution of Just answer -> show (getSum answer)
                             Nothing     -> "no answer found!")

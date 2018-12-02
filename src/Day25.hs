module Day25 (solveDay) where

import Prelude hiding (readFile, readList)
import Paths_advent_of_code

import           Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.IO (readFile)
import           Data.Text.Read (decimal, signed)


type Frequency = Sum Int

mkFrequency :: Int -> Frequency
mkFrequency = Sum


readList :: FilePath -> IO [Frequency]
readList = fmap (map (parseInt . T.strip) . T.lines) . readFile


parseInt:: T.Text -> Frequency
parseInt txt = case signed decimal txt of
  Left _ -> mempty
  Right (i, _) -> mkFrequency i


findFirstRepetition :: Set.Set Frequency -> [Frequency] -> Maybe Frequency
findFirstRepetition _ [] = Nothing
findFirstRepetition seen (f:fs) = case f `Set.member` seen of True -> Just f
                                                              False -> findFirstRepetition  (f `Set.insert` seen) fs

findFirstFrequencyRepetition :: Frequency -> [Frequency] -> Maybe Frequency
findFirstFrequencyRepetition initial fs = findFirstRepetition Set.empty fStream
  where fStream = scanl (+) initial (cycle fs)


solveDay :: IO ()
solveDay = do
  pth <- getDataFileName "data/day25.txt"
  fs <- readList pth
  let solution  = findFirstFrequencyRepetition mempty fs
  putStrLn $ (case solution of Just answer -> show (getSum answer)
                               Nothing     -> "no answer found!")

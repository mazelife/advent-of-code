module Lib
    ( solveDays
    ) where


import qualified Day24
import qualified Day25


solveDays :: IO ()
solveDays = do
  putStrLn "Solving day 25. Answer is:" >> Day25.solveDay
  putStrLn "Solving day 24. Answer is:" >> Day24.solveDay


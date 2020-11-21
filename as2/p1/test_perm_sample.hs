module Main where

import Text.Printf (printf)
import System.Timeout (timeout)

import Perm

max_score :: Float
max_score = 3.0

main = grade inputs

-- input, output, is_stack_permutation
inputs :: [([Int], [Int], Bool)]
inputs = [([1, 2, 3], [1, 2, 3], True),
          ([1, 2, 3], [2, 3, 1], False)]

grade :: [([Int], [Int], Bool)] -> IO ()
grade inputs = grade' inputs 0 0

grade' :: [([Int], [Int], Bool)] -> Int -> Int -> IO ()
grade' [] correct total = do
  putStr $ printf "Total tests: %d/%d\n" correct total
  putStr $ printf "\nGrade: %.1f / %.1f\n" score max_score
    where
      score :: Float
      score = max_score * (fromIntegral correct) / (fromIntegral total)
grade' ((input, output, is_perm) : rest) correct total = do
  result <- timeout (2*10^6) $ return $! perm input output
  putStr $ printf "Test %2d: " (total + 1)
  case result of
    Nothing -> do
      putStrLn "Timeout after 2 seconds"
      grade' rest correct (total + 1)
    Just result ->
      if result == is_perm then
        do
          putStrLn "SUCCESS"
          grade' rest (correct + 1) (total + 1)
      else
        do
          putStrLn "FAILURE"
          grade' rest correct (total + 1)

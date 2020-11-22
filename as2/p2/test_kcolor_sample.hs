module Main where

import Text.Printf (printf)
import System.Timeout (timeout)

import Data.List
import qualified Data.Set as Set

import Kcolor

maxScore :: Float
maxScore = 2.0

main = grade inputs

grade :: [([(Int, [Int])], Int, Bool)] -> IO ()
grade inputs = grade' inputs 0 0

grade' :: [([(Int, [Int])], Int, Bool)] -> Int -> Int -> IO ()
grade' [] correct total = do
  putStr $ printf "Total tests: %d/%d\n" correct total
  putStr $ printf "\nGrade: %.1f / %.1f\n" score maxScore
  putStrLn $ "WARNING: This sample program DOES NOT ACTUALLY TEST YOUR CODE"
    where
      score :: Float
      score = maxScore * (fromIntegral correct) / (fromIntegral total)
grade' ((input, k, isPossible) : rest) correct total = do
  result <- timeout (60*10^6) $ return $! kcolor input k
  putStr $ printf "Test %2d: " (total + 1)
  case result of -- TODO
    Nothing -> do
      putStrLn "FAILURE -- Timeout after 60 seconds"
      grade' rest correct (total + 1)
    Just result' ->
      case result' of
        Nothing -> do
          case isPossible of
            True -> do
              putStrLn "[FAILURE] -- Failed to find a solution"
              grade' rest correct (total + 1)
            False -> do
              putStrLn "[SUCCESS]"
              grade' rest (correct + 1) (total + 1)
        Just coloring -> do
          case checkSolution (input, k, isPossible) coloring of
            True -> do
              putStrLn "[SUCCESS]"
              grade' rest (correct + 1) (total + 1)
            False -> do
              putStrLn "[FAILURE] -- Incorrect coloring"
              grade' rest correct (total + 1)


checkSolution :: ([(Int, [Int])], Int, Bool) -> [(Int, Char)] -> Bool
checkSolution (graph, k, possible) coloring = True

inputs :: [([(Int, [Int])], Int, Bool)]
inputs = [
  -- Small tests
  ([(1, [2, 3]), (2, [1, 3]), (3, [1, 2])], 3, True),
  ([(1, [2, 3]), (2, [1, 3]), (3, [1, 2])], 1, False)
  ]

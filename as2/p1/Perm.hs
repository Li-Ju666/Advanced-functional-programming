module Perm(perm) where

import Test.QuickCheck

perm:: [Int] -> [Int] -> Bool
perm input output = permS (reverse input) (reverse output) []

permS:: [Int] -> [Int] -> [Int] -> Bool
permS [] output stack = 
    case output of 
        [] -> True
        (o:os) -> if checkOutput o stack then permS [] os (tail stack)
                                         else False
    where checkOutput num [] = False
          checkOutput num (s:ss) = if num == s then True else False

permS (i:is) (o:os) []
    | i == o = permS is os []
    | otherwise = permS is (o:os) [i]

permS input@(i:is) output@(o:os) stack@(s:ss)
    | i == o = permS is os stack 
    | s == o = permS input os ss
    | otherwise = permS is output (i:stack)

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

-- ==============================================================
-- Property-based test for perm:

-- property 1: for dequeuing order i j k, if i<j<k while Qk<Qi<Qj, 
-- the input queue is not stack-sortable
prop_unsortable :: [Int] -> [Int] -> Bool
prop_unsortable = undefined

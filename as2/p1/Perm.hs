module Perm(perm) where

import Test.QuickCheck
import Data.List(sort, nub)

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

-- ==============================================================
-- Property-based test for perm:

-- property 1: for dequeuing order i j k, if i<j<k while Qk<Qi<Qj, 
-- the input queue is not stack-sortable
prop_unsortable x = (x>3) ==> (pattern_test [x, x-1 .. 1])

pattern_test :: [Int] -> Bool
pattern_test xs = False == perm (reverse q231) xs
    where q231 = generate231(xs)

generate231 :: [Int] -> [Int]
generate231 (x1:x2:x3:xs) = mid:max:min:xs
    where [min, mid, max] = sort [x1, x2, x3]

-- Property 2: lists with less than 1,000,000 elements can 
-- be computed with in one second
prop_eff x = (x>0 and x<1000000) ==> eff_test [x,x-1..0]
    where types = x::Int

-- eff_test xs = 


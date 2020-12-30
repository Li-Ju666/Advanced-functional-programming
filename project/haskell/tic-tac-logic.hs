import Data.Char
import Data.List
-- import Test.QuickCheck

main = do
    allContents <- getContents
    let contents = tail (lines allContents)
    let Just result = solve contents
    -- putStr $ joinStr rowContents
    -- putStr "\n"
    putStr $ displayBoard result 

------------------- Haskell Unique Features Used: 
-- I. Maybe monad: if an incorrect guess, Nothing will be returned, 
--  otherwise, correct results will be in Just monad
-- II. Lazy evaluation: when guessing, 


------------------------ Core functions ---------------------------
-- Solve function: apply multiple board-wise rules
solve :: [[Char]] -> Maybe [[Char]]
solve board = 
    if filledBoard result
        then (if solved result then Just result else Nothing) 
        else (if board==result then guess result else solve result)
    where 
        result = avoidDupLine $ 
            lineRules board
                -- avoidDupLine $ 
                -- completeLine $ avoidTri2 $ avoidTri1 board

guess :: [[Char]] -> Maybe [[Char]]
guess board = 
    if tryWithX == Nothing then tryWithO else tryWithX 
    where
        -- approach 1: guess one cell in the row with least empty cells
        empties = map unfilledLine board
        target = head [x | x<-sort empties, x>0]
        Just tryIdx = elemIndex target empties
        guessWithX = take tryIdx board ++ [dotRep "X" (board!!tryIdx)] ++
                drop (tryIdx+1) board
        guessWithO = take tryIdx board ++ [dotRep "O" (board!!tryIdx)] ++
                drop (tryIdx+1) board
        -- approach 2: guess the first empty cell
        -- guessWithX = lines $ dotRep "X" (unlines board)
        -- guessWithO = lines $ dotRep "O" (unlines board)
        tryWithX = solve guessWithX
        tryWithO = solve guessWithO

---------------- helper functions for board ----------------------
-- to check if the board has been filled
filledBoard :: [[Char]] -> Bool
filledBoard board = not $ any (True==) (map (any ('.'==)) board)

unfilledBoard :: [[Char]] -> Int
unfilledBoard board = sum $ map unfilledLine board

-- to check if the board is a valid solution
solved :: [[Char]] -> Bool
solved rowBoard = 
    and [--filledBoard rowBoard:
        length rowBoard == length (nub rowBoard), -- no duplicate row
        length colBoard == length (nub colBoard), -- no duplicate col
        and $ map lineCheck rowBoard, -- even symbol in each row 
        and $ map lineCheck colBoard]  -- even symbol in each col
    where
        colBoard = transpose rowBoard
        lineCheck = \line -> halfSymbol line && noTriInline line

-------------------- helper functions for each line ----------------
-- helper function to check if a line is filled or not
filledLine :: [Char] -> Bool
filledLine l = unfilledLine l == 0

-- helper function to check if a line has no triple symbols
noTriInline :: [Char] -> Bool
noTriInline l = and [all ((/=) "XXX") lWithAdjs, 
                      all ((/=) "OOO") lWithAdjs]
    where lWithAdjs = withAdjs l []

-- helper function to check if number of one type of symbol in a 
-- line exceed half of length of the line
halfSymbol :: [Char] -> Bool
halfSymbol l = 
    2*length (filter (\x -> x=='X') l) <= length l && 
    2*length (filter (\x -> x=='O') l) <= length l

-- helper function to check how many nodes are not filled in a line
unfilledLine :: [Char] -> Int
unfilledLine l = length $ filter ((==) '.') l

-- helper function to generate node with its neighbours
withAdjs (x1:x2:x3:xs) acc = withAdjs (x2:x3:xs) ([x1,x2,x3]:acc)
withAdjs otherwise acc = acc

-- to apply a technique to boards both row-wise and column-wise
rowColMap :: ([Char] -> [Char]) -> [[Char]] -> [[Char]]
rowColMap tech rowBoard = transpose colResult
    where
        rowResult = map tech rowBoard
        colResult = map tech (transpose rowResult)


---------------- rules and board-wise applying -----------------------
----------------------------------------------------------------------

-- 1. Line-wise rules

-- applier functions: 
-- 1). apply all line-wise rules for the board once
lineRules :: [[Char]] -> [[Char]]
lineRules board = rowColMap ((rule3 board).rule4.rule2.rule1) board

-- 2). recursively apply a list of rules to a line till convergence
rcsLineApply :: [([Char] -> [Char])] -> [Char] -> [Char]
rcsLineApply rules line = 
    if line == result then result else rcsLineApply rules result
    where result = (foldl (\acc rule -> acc.rule) id rules) line


-- rule 1: avoid triple 1 on each line
rule1 ::[Char] -> [Char]
-- rule1 l = (reverse.rule1d.reverse.rule1d) l
-- rule1 l = rule1d l

-- rule1 :: [Char] -> [Char]
rule1 ('X':'X':'.':xs) = "XX" ++ rule1 ('O':xs)
rule1 ('O':'O':'.':xs) = "OO" ++ rule1 ('X':xs)
rule1 ('.':'X':'X':xs) = 'O':rule1 ("XX"++xs)
rule1 ('.':'O':'O':xs) = 'X':rule1 ("OO"++xs)
-- rule1d (x1:x2:x3:xs) = 
--     if (and [x1==x2, x1/='.'])
--         then x1:x2:(rule1d ((opo x1):xs))
--         else x1:(rule1d (x2:x3:xs))
rule1 (x:xs) = x:rule1 xs
rule1 [] = []

-- rule 2: avoid triple 2 on each line 
rule2 :: [Char] -> [Char]
-- rule2 (x1:x2:x3:xs) = 
--     if (and [x1==x3, x2=='.', x1/='.'])
--         then x1:(opo x1):(rule2 (x3:xs))
--         else x1:(rule2 (x2:x3:xs))
rule2 ('X':'.':'X':xs) = "XO"++rule2 ('X':xs)
rule2 ('O':'.':'O':xs) = "OX"++rule2 ('O':xs)
rule2 (x:xs) = x:rule2 xs
rule2 [] = []

-- rule 3: avoid triple 3 and duplicate line in board on each line 
rule3 :: [[Char]] -> [Char] -> [Char]
rule3 board line = -- filter checkLine allPoss 
    case lastOne of
        '.' -> line
        _ -> case take 1 $ filter checkLine allPoss of
            [] -> line
            [invalid] -> dotRep (map 
                (\x->if x=='.' then x else opo x) invalid) line
    where
        lastOne = 
            if (occurance 'X' line + 1)==(length line `div` 2)
            then 'X'
            else if (occurance 'O' line + 1)==(length line `div` 2)
                then 'O'
                else '.'
        occurance x = length.(filter (x==))
        empties = unfilledLine line
        allPoss = map (\x -> 
            (take x $ repeat '.') ++ [lastOne]) [0..empties-1]
        checkLine = \poss -> 
            (invalidLine.(rcsLineApply [rule4])) $ 
                dotRep poss line
        invalidLine = \line -> not (noTriInline line) ||
            (elem line board)


-- rule 4: complete line
rule4 :: [Char] -> [Char]
rule4 line
    | length line == 2*length (filter ('X'==)  line) =
        dotRep (repeat 'O') line 
    | length line == 2*length (filter ('O'==) line) =
        dotRep (repeat 'X') line
    | otherwise = line


-----------------------------------------------------------------
-- 2. board-wise rules
-- rule 5: avoid duplicated row/column
avoidDupLine :: [[Char]] -> [[Char]]
avoidDupLine rowBoard = transpose colResult
    where
        rowEmpties = map unfilledLine rowBoard
        rowResult = if any ((==) 0) rowEmpties && any ((==) 2) rowEmpties
            then checkDup rowBoard rowBoard []
            else rowBoard
        colBoard = transpose rowResult
        colEmpties = map unfilledLine colBoard
        colResult = if any ((==) 0) colEmpties && any ((==) 2) colEmpties
            then checkDup colBoard colBoard []
            else colBoard

checkDup :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]]
checkDup [] board newBoard = newBoard 
checkDup (line:lines) board newBoard = 
    if unfilledLine line == 2 
        then checkDup lines board (newBoard++[result])
        else checkDup lines board (newBoard++[line])
    where
        xoRep = dotRep "XO" line
        oxRep = dotRep "OX" line
        result = if any ((==) xoRep) board
            then oxRep
            else if any ((==) oxRep) board
                then xoRep else line

-- rule A2: avoid potential duplicate row/column
-- avoidPotentialDup :: [[Char]] -> [[Char]]
-- avoidPotentialDup rowBoard = 
--     where 

-- replace '.' with a list of symbols
dotRep [] ss = ss
dotRep _ [] = []
dotRep (s:ss) (x:xs) = 
    if x=='.' 
        then (s:dotRep ss xs) 
        else x:(dotRep (s:ss) xs)
------------------- other helper functions -------------------------
-- helper function to get oposite symbol 
opo 'X' = 'O'
opo 'O' = 'X'
opo other = other

-- helper function to print board line by line
displayBoard :: [[Char]] -> [Char]
displayBoard x = foldl (\acc l -> acc++l) [] (map (\n -> n++"\n") x)

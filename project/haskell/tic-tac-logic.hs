import Data.Char
import Data.List

main = do
    allContents <- getContents
    let contents = tail (lines allContents)
    let result = solve contents
    -- putStr $ joinStr rowContents
    -- putStr "\n"
    putStr $ displayBoard result 




------------------------ Core functions ---------------------------
-- Solve function: apply multiple board-wise rules
solve :: [[Char]] -> [[Char]]
solve board = 
    if filled result
        then (if solved result then result else []) 
        else (if board==result then guess result else solve result)
    where result = fillLast $ avoidTri2 $ avoidTri1 board

guess :: [[Char]] -> [[Char]]
guess board = 
    -- if fstTry == [] then solve boardWithO else fstTry
    boardWithO
    where
        boardInline = unlines board
        Just tryIdx = elemIndex '.' boardInline
        boardWithX = 
            lines (take tryIdx boardInline ++
                ['X'] ++ drop (tryIdx+1) boardInline)
        boardWithO = 
            lines (take tryIdx boardInline ++
                ['O'] ++ drop (tryIdx+1) boardInline)
        fstTry = solve boardWithX


-- to check if the board has been filled
filled :: [[Char]] -> Bool
filled board = and (map lineCheck board)
    where lineCheck = \line -> and (map ((/=) '.') line)

-- to check if the board is a valid solution
solved :: [[Char]] -> Bool
solved rowBoard = 
    and ((length rowBoard == length (nub rowBoard)):
        (length colBoard == length (nub colBoard)):
        (map lineCheck rowBoard)++
        (map lineCheck colBoard))
    where
        colBoard = transpose rowBoard
        lineCheck = \line -> 
            2*length (filter (\x -> x=='X') line) == length line

rowColMap :: ([Char] -> [Char]) -> [[Char]] -> [[Char]]
-- to apply a technique to boards both row-wise and column-wise
rowColMap tech rowBoard =
    zipWith uniLine rowResult (transpose colResult)
    where
        rowResult = map tech rowBoard
        colResult = map tech (transpose rowBoard)
        uniLine = \line1 line2 -> zipWith (\w1 w2 -> 
            if w1==w2 then w1 else (max w1 w2)) line1 line2


---------------- rules and board-wise applying -----------------------
-- apply rule 1 for the board
avoidTri1 :: [[Char]] -> [[Char]]
avoidTri1 board = rowColMap rule1 board

-- rule 1: avoid triple 1 on each line
rule1 ::[Char] -> [Char]
rule1 l = reverse $ rule1d (rule1d $ reverse (rule1d l))

rule1d :: [Char] -> [Char]
rule1d (x1:x2:x3:xs) = 
    if (and [x1==x2, x1/='.'])
        then x1:x2:(rule1d ((opo x1):xs))
        else x1:(rule1d (x2:x3:xs))
rule1d others = others


-- apply rule 2 for the board
avoidTri2 :: [[Char]] -> [[Char]]
avoidTri2 board = rowColMap rule2 board

-- rule 2: avoid triple 2 on each line
rule2 (x1:x2:x3:xs) = 
    if (and [x1==x3, x2=='.', x1/='.'])
        then x1:(opo x1):(rule2 (x3:xs))
        else x1:(rule2 (x2:x3:xs))
rule2 others = others

-- apply rule 4 for the board
fillLast :: [[Char]] -> [[Char]]
fillLast board = rowColMap rule4 board

rule4 line = case ['.'] == (filter ((==) '.') line) of
    True -> map (\x -> if x=='.' then lastElem else x) line
    False -> line
    where
        lastElem = if 2*length (filter ((==) 'X') line) < length line
            then 'X' else 'O'
        

------------------- other helper functions -------------------------
-- helper function to get oposite symbol 
opo 'X' = 'O'
opo 'O' = 'X'
opo other = other

-- helper function to print board line by line
displayBoard :: [[Char]] -> [Char]
displayBoard x = foldl (\acc l -> acc++l) [] (map (\n -> n++"\n") x)

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
    where result = 
                avoidDupLine $ completeLine $ -- avoidTri3 $
                avoidTri2 $ avoidTri1 board
                -- avoidDupLine $ 
                -- completeLine $ avoidTri2 $ avoidTri1 board

guess :: [[Char]] -> [[Char]]
guess board = 
    if fstTry /= [] then fstTry else solve boardWithO
    -- boardWithO
    where
        -------- try on least empty line
        empties = map unfilledInline board
        target = head [x | x<-sort empties, x>0]
        Just tryIdx = elemIndex target empties
        boardWithX = take tryIdx board ++ [repOnce 'X' (board!!tryIdx)] ++
                drop (tryIdx+1) board
        boardWithO = take tryIdx board ++ [repOnce 'O' (board!!tryIdx)] ++
                drop (tryIdx+1) board
        fstTry = solve boardWithX


-- to check if the board has been filled
filled :: [[Char]] -> Bool
filled board = and (map lineCheck board)
    where lineCheck = \line -> if unfilledInline line == 0
                                    then True else False

-- to check if the board is a valid solution
solved :: [[Char]] -> Bool
solved rowBoard = 
    and ((length rowBoard == length (nub rowBoard)):
        (length colBoard == length (nub colBoard)):
        (map lineCheck rowBoard)++
        (map lineCheck colBoard)++
        (map noTriInline rowBoard)++
        (map noTriInline colBoard))
    where
        colBoard = transpose rowBoard
        lineCheck = \line -> 
            2*length (filter (\x -> x=='X') line) == length line


noTriInline :: [Char] -> Bool
noTriInline l = and [all ((/=) "XXX") lWithAdjs, 
                      all ((/=) "OOO") lWithAdjs]
    where lWithAdjs = withAdjs l []

-- helper function to check how many nodes are not filled in a line
unfilledInline :: [Char] -> Int
unfilledInline l = length $ filter ((==) '.') l

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

-- apply rule 3 for the board
avoidTri3 :: [[Char]] -> [[Char]]
avoidTri3 board = rowColMap rule3 board

-- rule 3: avoid triple 3 on each line
rule3 line =
    case unfilledInline line of
        3 -> case [noTriInline (rule4 repFst), 
                    noTriInline (rule4 repLst)] of 
                [True, True] -> line
                [False, True] -> repOnce two line
                [True, False] -> reverse $ repOnce two (reverse line)
                otherwise -> line 
        otherwise -> line
    where
        one = if 2*((length $ filter ((==) 'X') line) + 1) == length line
                    then 'X' else 'O'
        two = opo one
        repFst = repOnce one line
        repLst = reverse $ repOnce one (reverse line)  


-- apply rule 4 for the board
completeLine :: [[Char]] -> [[Char]]
completeLine board = rowColMap rule4 board

rule4 line 
    | length line == 2*length (filter ((==) 'X') line) =
        map (\x -> if x=='.' then 'O' else x) line
    | length line == 2*length (filter ((==) 'O') line) =
        map (\x -> if x=='.' then 'X' else x) line
    | otherwise = line


-- apply rule 5 for the board: avoid duplicated row/colnumn
avoidDupLine :: [[Char]] -> [[Char]]
avoidDupLine rowBoard = transpose colResult
    where
        rowResult = if any ((==) 0) (map unfilledInline rowBoard) 
            then checkDup rowBoard rowBoard []
            else rowBoard
        colBoard = transpose rowResult
        colResult = if any ((==) 0) (map unfilledInline colBoard)
            then checkDup colBoard colBoard []
            else colBoard


checkDup :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]]
checkDup [] board newBoard = newBoard 
checkDup (line:lines) board newBoard = 
    if unfilledInline line == 2 
        then checkDup lines board (newBoard++[result])
        else checkDup lines board (newBoard++[line])
    where
        xoRep = \line -> repOnce 'O' (repOnce 'X' line)
        oxRep = \line -> repOnce 'X' (repOnce 'O' line)
        result = if any ((==) (xoRep line)) board
            then oxRep line
            else if any ((==) (oxRep line)) board
                then xoRep line else line

-- place first empty node with given symbol in a line
repOnce a (x:xs) = if x=='.' then a:xs else x:(repOnce a xs)
repOnce a [] = [] 
------------------- other helper functions -------------------------
-- helper function to get oposite symbol 
opo 'X' = 'O'
opo 'O' = 'X'
opo other = other

-- helper function to print board line by line
displayBoard :: [[Char]] -> [Char]
displayBoard x = foldl (\acc l -> acc++l) [] (map (\n -> n++"\n") x)

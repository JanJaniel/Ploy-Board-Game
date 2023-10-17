module Ploy where  -- do NOT CHANGE export of module

import Board

import Data.Bits ( (.&.), (.|.), shift )
import Data.List 
import Data.Maybe

data Move = Move {start :: Pos, target :: Pos, turn :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ (show startR) ++ "-" ++ [tarC] ++ show tarR ++ "-" ++ show tr

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 


hasBlackPiece :: Cell -> Bool
hasBlackPiece (Piece Black _) = True
hasBlackPiece _               = False

hasWhitePiece :: Cell -> Bool
hasWhitePiece (Piece White _) = True
hasWhitePiece _               = False

countBlackPieces :: Board -> Int
countBlackPieces b = length (filter hasBlackPiece  (concat b) )

countWhitePieces :: Board -> Int
countWhitePieces b = length ( filter hasWhitePiece  (concat b) )
 
gameFinished :: Board -> Bool
gameFinished b  
  | let
     x = length (concat (map (filter (== Piece Black 170)) b)) 
     y = length (concat (map (filter (== Piece Black 85)) b)) 
     in x == 0 && y == 0 = True 
  | let 
    x = length (concat (map (filter (== Piece White 170)) b)) 
    y = length (concat (map (filter (== Piece White 85)) b)) 
    in x == 0 && y == 0 = True  
  | countBlackPieces b == 1 = True
  | countWhitePieces b == 1 = True
  | otherwise = False


isValidMove :: Board -> Move -> Bool
isValidMove board m = 
  let 
    pathIsCorrect = targetOnPath m 
    targetCellCorrect = isMoveMoving ((start m) == (target m)) board m 
    noBlockedPath = isPathNotBlocked (line (start m) (target m)) board 
    directionCorrect = isMoveDirectionValid m board
  in pathIsCorrect && targetCellCorrect && noBlockedPath && directionCorrect


-- is not valid if the target is not on a possible line path 
targetOnPath :: Move -> Bool
targetOnPath m 
   | col (start m) == col (target m) = True 
   | row (start m) == row (target m)  = True
   | calculateLengthCol (col(start m)) (col(target m)) == calculateLengthRow (row(start m)) (row(target m)) = True
   | otherwise = False

calculateLengthCol :: Char -> Char -> Int
calculateLengthCol s t 
  | s < t  = length [s .. t]
  | s > t  =  length [t .. s]
  | s == t = 1

calculateLengthRow :: Int -> Int -> Int
calculateLengthRow s t 
  | s < t  = length [s .. t]
  | s > t  =  length [t .. s]
  | s == t = 1


-- 2 Variants entweder es bewegt sich nicht oder es bewegt sich 
isMoveMoving :: Bool -> Board -> Move ->  Bool 
isMoveMoving b board m
  | b == True = True
  | b == False = (isCellEmpty (makeCell board (target m))) || (isCellEnemy (makeCell board (start m )) (makeCell board (target m)))

-- is valid if target pos is empty 
isCellEmpty :: Cell -> Bool
isCellEmpty c 
  | c == Empty = True
  | otherwise = False 

-- is valid if target is enemy 
isCellEnemy :: Cell -> Cell-> Bool
isCellEnemy s t 
  | hasBlackPiece s  == True && hasBlackPiece t == True = False
  | hasWhitePiece s == True && hasWhitePiece t == True = False
  | otherwise = True

-- is a piece blocking the way ?
isPathNotBlocked :: [Pos] -> Board-> Bool
isPathNotBlocked pl b =
    let x = map (makeCell b) pl 
        y = firstLast x
        z = length(filter (not . isCellEmpty) y)
    in z < 1

firstLast::[a]->[a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)

-- is not valid when a piece wants to make a move which is illegal (target has to Be on path)
isMoveDirectionValid :: Move -> Board -> Bool
isMoveDirectionValid m b
 | targetOnPath m == False = False 
 | numberOfSteps m == 0 = True 
 | let x = cellToInt (makeCell b (start m)) in x!!7 == 1 &&  row (start m) < row (target m) && col (start m) == col (target m) = True 
 | let x = cellToInt (makeCell b (start m)) in x!!6 == 1 &&  row (start m) < row (target m) && col (start m) < col (target m) = True 
 | let x = cellToInt (makeCell b (start m)) in x!!5 == 1 &&  row (start m) == row (target m) && col (start m) < col (target m) = True 
 | let x = cellToInt (makeCell b (start m)) in x!!4 == 1 &&  row (start m) > row (target m) && col (start m) < col (target m) = True
 | let x = cellToInt (makeCell b (start m)) in x!!3 == 1 &&  row (start m) > row (target m) && col (start m) == col (target m) = True
 | let x = cellToInt (makeCell b (start m)) in x!!2 == 1 &&  row (start m) > row (target m) && col (start m) > col (target m) = True
 | let x = cellToInt (makeCell b (start m)) in x!!1 == 1 &&  row (start m) == row (target m) && col (start m) > col (target m) = True
 | let x = cellToInt (makeCell b (start m)) in x!!0 == 1 &&  row (start m) < row (target m) && col (start m) > col (target m) = True 
 | otherwise = False

numberOfSteps :: Move -> Int
numberOfSteps m 
  | start m == target m = 0
  | otherwise =  length (line (start m) (target m)) -1

fillBinListZero :: [Int] -> [Int]
fillBinListZero i
  | length i < 8 = let x = replicate (8 - length i) 0 in x ++ i
  | otherwise = i

--cell to binary notation (I need to change the function name)
cellToInt :: Cell -> [Int]   
cellToInt (Piece p i) =  fillBinListZero(decToBin i)

turnColintoInt :: Char -> Int
turnColintoInt c = 
  let x = ['a'.. c] 
  in (length x ) - 1

makeCell :: Board -> Pos -> Cell 
makeCell b p = 
  let 
    x = 9 - (row p)
    y = turnColintoInt (col p )
    cell = (b!!x)!!y
  in cell


whatTypeIsPiece :: Cell -> String
whatTypeIsPiece c 
  | let x = convertPlayerInt c in x == 1 = "Shield"
  | let x = convertPlayerInt c in x == 2 = "Probe"
  | let x = convertPlayerInt c in x == 3 = "Lance"
  | let x = convertPlayerInt c in x == 4 = "Commander"
  
decToBin :: Int -> [Int]
decToBin = go [] where
   go acc 0 = acc
   go acc n = let (d, m) = n `divMod` 2 in go (m : acc) d

convertPlayerInt :: Cell -> Int
convertPlayerInt (Piece p i) = 
  let x = decToBin i 
  in length (filter (==1) x )

turnIntToChar :: Int -> Char
turnIntToChar i = let x = ['a'..'z'] in x!!i

possibleMoves :: Pos -> Cell -> [Move] 
possibleMoves p c 
  | whatTypeIsPiece c == "Shield" = makeMoveListShield p (getPosList c p 1) [0..7]
  | whatTypeIsPiece c == "Probe"  = makeMoveListProbe c p (getPosList c p 2) [0..7]
  | whatTypeIsPiece c == "Lance"  = makeMoveListLance c p (getPosList c p 3) [0..7]
  | whatTypeIsPiece c == "Commander" = makeMoveListCommander p (getPosList c p 1) [0..7]


makeMoveListShield :: Pos -> [Pos] -> [Int] ->[Move]
makeMoveListShield p pl il = 
  let 
    x = [Move p x y | x <- pl, y <- il]
    result = tail x 
  in result

makeMoveListProbe :: Cell -> Pos -> [Pos] -> [Int] -> [Move]  
makeMoveListProbe c p pl il 
  | whatTypeOfProbe c == "ProbeA" || whatTypeOfProbe c == "ProbeB" =  
      let 
        ntl = filter (/= p) pl
        i = head il
        result1 = [Move p z i | z <- ntl]
        result2 = [Move p p z| z <- (tail il)]
      in result1 ++ result2 
  | whatTypeOfProbe c == "ProbeC" = 
      let
        ntl = filter (/= p) pl
        i = head il
        result1 = [Move p z i | z <- ntl]
        result2 = [Move p p z| z <- (filter (/= 4) (tail il))]
      in  result1 ++ result2 

makeMoveListLance :: Cell -> Pos ->[Pos] -> [Int] -> [Move]
makeMoveListLance c p pl il = 
    let 
        ntl = filter (/= p) pl
        i = head il
        result1 = [Move p z i | z <- ntl]
        result2 = [Move p p z| z <- (tail il)]
    in result1 ++ result2 
    
makeMoveListCommander :: Pos -> [Pos] -> [Int] -> [Move]
makeMoveListCommander p pl il =
      let
        ntl = filter (/= p) pl
        i = head il
        result1 = [Move p z i | z <- ntl]
        result2 = [Move p p z| z <- (filter odd il)]
      in  result1 ++ result2 

-- creates list of reachable positions 
getPosList ::  Cell -> Pos -> Int -> [Pos] 
getPosList c p i = 
  let 
    x = cellToInt c 
    n = if x!!7 == 1 then getPosListN i p else []
    ne = if x!!6 == 1 then getPosListNE i p else []
    e = if x!!5 == 1 then getPosListE i p else []
    se = if x!!4 == 1 then getPosListSE i p else []
    s = if x!!3 == 1 then getPosListS i p else []
    sw = if x!!2 == 1 then getPosListSW i p else []
    w = if x!!1 == 1 then getPosListW i p else []
    nw = if x!!0 == 1 then getPosListNW i p else []
  in n ++ ne ++ e ++ se ++ s ++ sw ++ w ++ nw

whatTypeOfProbe :: Cell -> String 
whatTypeOfProbe c 
  | let 
      bl = cellToInt c 
      x = fromJust (elemIndex 1 bl)
      nL = splitAt (x+1) bl
      y = fromJust (elemIndex 1 (snd nL))
    in y == 0 || y == 6 = "ProbeA"
  | let  
      bin = cellToInt c 
      x = fromJust (elemIndex 1 bin)
      nL = splitAt (x+1) bin
      y = fromJust (elemIndex 1 (snd nL))
    in y == 1 || y == 5 = "ProbeB"
  | let 
      bin = cellToInt c 
      x = fromJust (elemIndex 1 bin)
      nL = splitAt (x+1) bin
      y = fromJust (elemIndex 1 (snd nL))
    in y == 3 = "ProbeC"

whatTypeOfLance :: Cell -> String
whatTypeOfLance c 
  |let 
      bl = cellToInt c 
      x = fromJust (elemIndex 1 bl)
      nL = splitAt (x+1) bl
      y = fromJust (elemIndex 1 (snd nL))
    in y == 0 || y == 5 = "LanceA"
  |let 
      bl = cellToInt c 
      x = fromJust (elemIndex 1 bl)
      nL = splitAt (x+1) bl
      y = fromJust (elemIndex 1 (snd nL))
      nnL = splitAt (y+1) (snd nL)
      z = fromJust (elemIndex 1 (snd nnL))
    in z == 2 || (y == 2 && z == 1)= "LanceB"
  | otherwise = "LanceC"


getPosListN :: Int -> Pos -> [Pos]  
getPosListN 0 p = [p]
getPosListN i p = if (check p i )  == True then getPosListN (i-1) p ++ [Pos (col p) ((row p) + i)] else getPosListN (i-1) p
                        where
                          check :: Pos -> Int -> Bool 
                          check p i 
                            | ((row p) + i) <= 9 = True
                            | otherwise = False

getPosListNE :: Int -> Pos -> [Pos]
getPosListNE 0 p = [p]
getPosListNE i p = if (check p i )  == True then getPosListNE (i-1) p ++ [Pos (turnIntToChar ((turnColintoInt (col p)) +i)) ((row p) + i)] else getPosListNE (i-1) p
                        where 
                          check :: Pos -> Int -> Bool 
                          check p i 
                            | ((row p) + i) <= 9  && ((turnColintoInt (col p)) +i) <= 8 = True
                            | otherwise = False

getPosListE :: Int -> Pos -> [Pos]
getPosListE 0 p = [p]
getPosListE i p = if (check p i )  == True then getPosListE (i-1) p ++ [Pos (turnIntToChar ((turnColintoInt (col p)) +i)) (row p) ] else getPosListE (i-1) p
                         where 
                          check :: Pos -> Int -> Bool 
                          check p i 
                            | ((turnColintoInt (col p)) +i) <= 8 = True
                            | otherwise = False

getPosListSE :: Int -> Pos -> [Pos]
getPosListSE 0 p = [p]
getPosListSE i p = if (check p i )  == True then getPosListSE (i-1) p ++ [Pos (turnIntToChar ((turnColintoInt (col p)) +i)) ((row p) - i)] else getPosListSE (i-1) p
                          where 
                          check :: Pos -> Int -> Bool 
                          check p i 
                            | ((turnColintoInt (col p)) +i) <= 8 && ((row p) - i) >= 1 = True
                            | otherwise = False


getPosListS :: Int -> Pos -> [Pos]
getPosListS 0 p = [p]
getPosListS i p = if (check p i )  == True then getPosListS (i-1) p ++ [Pos (col p) ((row p) - i)] else getPosListS (i-1) p
                          where 
                          check :: Pos -> Int -> Bool 
                          check p i 
                            | ((row p) - i) >= 1 = True
                            | otherwise = False


getPosListSW :: Int -> Pos -> [Pos]
getPosListSW 0 p = [p]
getPosListSW i p =  if (check p i )  == True then getPosListSW (i-1) p ++ [Pos (turnIntToChar ((turnColintoInt (col p)) -i)) ((row p) - i)] else getPosListSW (i-1) p
                          where 
                          check :: Pos -> Int -> Bool 
                          check p i 
                            | ((turnColintoInt (col p)) -i) >= 0 && ((row p) - i) >= 1 = True
                            | otherwise = False


getPosListW :: Int -> Pos -> [Pos]
getPosListW 0 p = [p]
getPosListW i p = if (check p i )  == True then getPosListW (i-1) p ++ [Pos (turnIntToChar ((turnColintoInt (col p)) -i)) (row p) ] else getPosListW (i-1) p
                          where 
                          check :: Pos -> Int -> Bool 
                          check p i 
                            | ((turnColintoInt (col p)) -i) >= 0 = True
                            | otherwise = False

getPosListNW :: Int -> Pos -> [Pos]
getPosListNW 0 p = [p]
getPosListNW i p = if (check p i )  == True then  getPosListNW (i-1) p ++ [Pos (turnIntToChar ((turnColintoInt (col p)) -i)) ((row p) + i)] else getPosListNW (i-1) p
                          where 
                          check :: Pos -> Int -> Bool 
                          check p i 
                            | ((turnColintoInt (col p)) -i) >= 0 && ((row p) + i) <= 9 = True
                            | otherwise = False


listMoves :: Board -> Player -> [Move]
listMoves b p 
  | gameFinished b == True = []
  | p == White = 
    let 
      cl = filter hasWhitePiece (concat b) 
      y = posFactory b cl 
      pm = makePossibleMoveList y
      vml = makeValidMoveList pm b
    in vml
  | p == Black = 
    let 
      cl = filter hasBlackPiece (concat b) 
      y = posFactory b cl
      pm = makePossibleMoveList y
      vml = makeValidMoveList pm b
    in vml


posFactory :: [[Cell]] -> [Cell] -> [(Cell,Pos)]
posFactory b [] = []
posFactory b (x:xs) =  
      let
        temp = findPosition x b 
        newBoard = map (repOcc x Empty) b 
        result = makeTriple temp x

      in result ++ posFactory newBoard xs 
      
makeTriple :: [(Int,Int)] -> Cell -> [(Cell,Pos)]
makeTriple il c = [(c,Pos (turnIntToChar y) (9 - x)) | (x,y) <- il]

findPosition :: Eq a => a -> [[a]] -> [(Int, Int)]
findPosition n xs = fp n xs 0
fp n [] i = []
fp n (x:xs) i = p x ++ fp n xs (i+1)
    where 
      p x = zip (repeat i) (elemIndices n x)

repOcc :: Eq t => t -> t -> [t] -> [t]
repOcc x y ls = map (\h -> if h == x then y else h) ls

makePossibleMoveList :: [(Cell,Pos)] -> [Move]
makePossibleMoveList [] = []
makePossibleMoveList (x:xs) = possibleMoves (snd x) (fst x) ++ makePossibleMoveList xs

makeValidMoveList :: [Move] -> Board -> [Move]
makeValidMoveList [] b = [] 
makeValidMoveList ml b = if isValidMove b (head ml) == True then [(head ml)] ++ makeValidMoveList (tail ml) b else makeValidMoveList (tail ml) b
                           
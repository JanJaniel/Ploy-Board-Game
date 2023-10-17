module Board where  -- do NOT CHANGE export of module


import Data.List
import Data.List.Split



data Player = Black | White deriving Show
data Cell = Piece Player Int | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Black Black = True
  (==) White White = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Piece p1 i1) (Piece p2 i2) = p1 == p2 && i1 == i2 
  (==) _ _ = False

validateFEN :: String -> Bool
validateFEN [] = False 
validateFEN a     
    | head a == '/' = False
    | last a == '/' = False
    | length (splitOn "/" a) /= 9 = False 
    | filter (/= 9) (map (length) (map (splitOn ",") (splitOn "/" a))) /= []  = False 
    | validatePieces a /= True = False
    | otherwise = True 

validColor :: Char -> Bool 
validColor x 
    |  x == 'b' = True
    |  x == 'w' = True
    |  otherwise = False

validNumber :: Integer -> Bool 
validNumber y
    | y < 1 = False
    | y > 255 = False
    | otherwise = True


splitOnAnyOf :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAnyOf ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

cutEmptyOut :: [[a]] -> [[a]] 
cutEmptyOut s = filter (not . null) s

stringToListOfTuples:: String -> [(Char,Integer)]
stringToListOfTuples s = stringToTuples (cutEmptyOut (splitOnAnyOf [",", "/"] s))
                            where
                                stringToTuples :: [String] -> [(Char,Integer)]
                                stringToTuples = map toTwoTuple 
                                    where
                                        toTwoTuple :: String -> (Char,Integer)
                                        toTwoTuple (x:xs) = (x,read xs :: Integer)
                                        toTwoTuple _ = undefined
    
validatePieces :: String -> Bool
validatePieces s =
    let color = fst (unzip (stringToListOfTuples s))
        number = snd (unzip (stringToListOfTuples s))
        boolListColor = map validColor color
        boolListNumber = map validNumber number
        lastCheckNumber = and boolListNumber
        lastCheckColor = and boolListColor
    in lastCheckColor && lastCheckNumber

buildBoard :: String -> Board
buildBoard s = 
    let x = formatString s 
        y = (map . map) convert x
    in y

processCell :: [Char] -> Cell 
processCell (x:xs) = 
    let farbe = processPlayer x
            where
                processPlayer :: Char -> Player
                processPlayer c 
                    | c == 'w' = White
                    | c == 'b' = Black 
        nummer = read xs :: Int
        result = Piece farbe nummer
    in result  

formatString :: String -> [[[Char]]]
formatString s = 
    let x = splitOn "/" s 
        y = map (splitOn ",") x
    in y

convert :: [Char] -> Cell
convert s 
        | length s == 0 = Empty
        | length s /= 0 = processCell s

line :: Pos -> Pos -> [Pos]
line start ziel 
    | start == ziel = [start]
    | let fall = welcherFall start ziel in fall == "horizontal" = makePosListh start ziel
    | let fall = welcherFall start ziel in fall == "vertical" = makePosListv start ziel
    | otherwise = makePosListd start ziel 

welcherFall :: Pos -> Pos -> String
welcherFall x y 
    | row x == row y = "horizontal"
    | col x == col y = "vertical"
    | otherwise = "diagonal"

toPosList :: (Char,Int) -> Pos
toPosList x = Pos (fst x) (snd x)
               
makePosListh :: Pos -> Pos -> [Pos]
makePosListh start ziel 
    | col start < col ziel = map toPosList (zip [col start .. col ziel] (take (length [col start .. col ziel]) $ repeat (row start)))
    | col start > col ziel = map toPosList ( zip (reverse [col ziel .. col start]) (take (length [col ziel .. col start]) $ repeat (row start)))

makePosListv :: Pos -> Pos -> [Pos]
makePosListv start ziel 
    | row start < row ziel = map toPosList (zip (take (length [row start .. row ziel]) $ repeat (col start)) [row start .. row ziel])
    | row start > row ziel = map toPosList (zip (take (length [row ziel .. row start]) $ repeat (col start)) (reverse[row ziel .. row start]))
    
makePosListd :: Pos -> Pos -> [Pos]
makePosListd start ziel 
    | col start < col ziel && row start < row ziel = map toPosList (zip [col start .. col ziel] [row start .. row ziel])
    | col start > col ziel && row start > row ziel = map toPosList (reverse (zip [col ziel .. col start] [row ziel .. row start]))
    | col start < col ziel && row start > row ziel = map toPosList (zip [col start .. col ziel] (reverse [row ziel .. row start]))
    | col start > col ziel && row start < row ziel = map toPosList (zip (reverse[col ziel .. col start]) [row start .. row ziel])

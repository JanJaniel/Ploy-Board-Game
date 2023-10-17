-- #############################################################################
-- ########### YOUR UNIT TESTS                                    ##############
-- ########### Note: execute tests using "stack test ploy:units"  ##############
-- #############################################################################
import Test.Hspec

import Board
    (validateFEN,
    buildBoard,
    line,
    Board,
    Cell(Empty, Piece),
    Player(Black, White),
    Pos(Pos))

import Ploy ( gameFinished, isValidMove, listMoves, Move(Move), possibleMoves )

main :: IO ()
main = hspec $ do
    testValidateFEN
    testBuildBoard
    testLine
    testGameFinished
    testIsValidMove
    testPossibleMoves
    testListMoves

startBoard :: Board
startBoard = [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

missingWCommanderBoard :: Board
missingWCommanderBoard = [[Empty,Piece White 84,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

missingBCommanderBoard :: Board
missingBCommanderBoard = [[Empty,Piece White 84,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

lastWhiteBoard :: Board
lastWhiteBoard = [[Empty,Empty,Empty,Empty,Piece White 170,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

lastBlackBoard :: Board
lastBlackBoard = [[Empty,Piece White 84,Piece White 41,Piece White 56,Empty,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Piece Black 170,Empty,Empty,Empty,Empty]]

testValidateFEN :: Spec 
testValidateFEN = describe "Module Board: validateFen ..." $ do 
        it "fen doesn't have 9 rows " $ do
            validateFEN ",,,,,,,,,/,,,,,,,,," `shouldBe` (False :: Bool)

        it "fen doesn't have 9 cols in each row" $ do 
            validateFEN ",,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (False :: Bool)

        it "fen has '/' as first character" $ do
            validateFEN "/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` (False :: Bool)
            
        it "fen has '/' as last character" $ do
            validateFEN ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/" `shouldBe` (False :: Bool)
        
        it "fen has illegal color" $ do 
            validateFEN ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,z1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69," `shouldBe` (False :: Bool)

        it "fen has illegal number" $ do 
            validateFEN ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w0,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69," `shouldBe` (False :: Bool)

testBuildBoard :: Spec
testBuildBoard = describe "Module Board: buildBoard ..." $ do
        it "build empty board" $ do
            buildBoard ",,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,," `shouldBe` [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]

        it "build starting board" $ do
            buildBoard ",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69," `shouldBe` [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

testLine :: Spec
testLine = describe "Module Board: line ..." $ do
        it "start is target" $ do
            line (Pos 'a' 1) (Pos 'a' 1) `shouldBe` ([(Pos 'a' 1)] :: [Pos])    

        it "horizontal line (col+)" $ do
            line (Pos 'e' 5) (Pos 'h' 5) `shouldBe` ([(Pos 'e' 5),(Pos 'f' 5),(Pos 'g' 5 ),(Pos 'h' 5 )] :: [Pos])

        it "horizontal line (col-)" $ do
            line (Pos 'e' 5) (Pos 'b' 5) `shouldBe` ([(Pos 'e' 5),(Pos 'd' 5),(Pos 'c' 5 ),(Pos 'b' 5 )] :: [Pos])
    
        it "vertical line (row+)" $ do
            line (Pos 'e' 5) (Pos 'e' 8) `shouldBe` ([(Pos 'e' 5),(Pos 'e' 6),(Pos 'e' 7),(Pos 'e' 8)] :: [Pos])

        it "vertical line (row-)" $ do
            line (Pos 'e' 5) (Pos 'e' 2) `shouldBe` ([(Pos 'e' 5),(Pos 'e' 4),(Pos 'e' 3),(Pos 'e' 2)] :: [Pos])

        it "diagonal line with: (col-) (row+)" $ do
            line (Pos 'e' 5) (Pos 'b' 8) `shouldBe` ([(Pos 'e' 5),(Pos 'd' 6),(Pos 'c' 7),(Pos 'b' 8)] :: [Pos])  

        it "diagonal line with: (col+) (row+)" $ do
            line (Pos 'e' 5) (Pos 'h' 8) `shouldBe` ([(Pos 'e' 5),(Pos 'f' 6),(Pos 'g' 7),(Pos 'h' 8)] :: [Pos])

        it "diagonal line with: (col+) (row-)" $ do
            line (Pos 'e' 5) (Pos 'h' 2) `shouldBe` ([(Pos 'e' 5),(Pos 'f' 4),(Pos 'g' 3),(Pos 'h' 2)] :: [Pos])

        it "diagonal line with: (col-) (row-)" $ do
            line (Pos 'e' 5) (Pos 'b' 2) `shouldBe` ([(Pos 'e' 5),(Pos 'd' 4),(Pos 'c' 3),(Pos 'b' 2)] :: [Pos])
        
        
testGameFinished :: Spec
testGameFinished = describe "Module Game: gameFinished ..." $ do
        it "white commander is missing" $ do
            gameFinished missingWCommanderBoard `shouldBe` (True :: Bool)  

        it "black commander is missing" $ do
            gameFinished missingBCommanderBoard `shouldBe` (True :: Bool)

        it "all white pieces eliminated except Commander" $ do
            gameFinished lastWhiteBoard `shouldBe` (True :: Bool)

        it "all black pieces eliminated except Commander" $ do
            gameFinished lastBlackBoard `shouldBe` (True :: Bool)     

testIsValidMove :: Spec
testIsValidMove = describe "Module Game: isValidMove ..." $ do
        it "rotation by 1 is always possible" $ do
            isValidMove startBoard (Move (Pos 'c' 1) (Pos 'c' 1) 1) `shouldBe` (True :: Bool)

        it "target is not on path" $ do
            isValidMove startBoard (Move (Pos 'f' 3) (Pos 'h' 5) 0) `shouldBe` (False :: Bool)

        it "target is a friendly cell" $ do
            isValidMove startBoard (Move (Pos 'f' 2) (Pos 'e' 3) 0) `shouldBe` (False :: Bool)
        
        it "move into right direction" $ do
            isValidMove startBoard (Move (Pos 'h' 1) (Pos 'h' 3) 0) `shouldBe` (True :: Bool)
            
        it "move into wrong direction" $ do
            isValidMove startBoard (Move (Pos 'f' 3) (Pos 'g' 3) 0) `shouldBe` (False :: Bool)
        
testPossibleMoves :: Spec
testPossibleMoves = describe "Module Game: possibleMoves ..." $ do
        it "move shield one step" $ do
            possibleMoves (Pos 'd' 3) (Piece White 1) `shouldContain` ([Move (Pos 'd' 3) (Pos 'd' 4) 0] :: [Move])

        it "move shield and turn 1 " $ do
            possibleMoves (Pos 'e' 5) (Piece White 1) `shouldContain` ([Move (Pos 'e' 5) (Pos 'e' 6) 1] :: [Move])

        it "moves ProbeA " $ do
            possibleMoves (Pos 'e' 5) (Piece White 3) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 6) 0),(Move (Pos 'e' 5) (Pos 'e' 7) 0),(Move (Pos 'e' 5) (Pos 'f' 6) 0),(Move (Pos 'e' 5) (Pos 'g' 7) 0)] :: [Move])

        it "turns ProbeA " $ do
            possibleMoves (Pos 'e' 5) (Piece White 3) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 1),(Move (Pos 'e' 5) (Pos 'e' 5) 2),(Move (Pos 'e' 5) (Pos 'e' 5) 3),(Move (Pos 'e' 5) (Pos 'e' 5) 4),(Move (Pos 'e' 5) (Pos 'e' 5) 5),(Move (Pos 'e' 5) (Pos 'e' 5) 6),(Move (Pos 'e' 5) (Pos 'e' 5) 7)] :: [Move])

        it "moves ProbeB" $ do
            possibleMoves (Pos 'e' 5) (Piece White 130) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'f' 6) 0),(Move (Pos 'e' 5) (Pos 'g' 7) 0),(Move (Pos 'e' 5) (Pos 'd' 6) 0),(Move (Pos 'e' 5) (Pos 'c' 7) 0)] :: [Move])

        it "turns ProbeB " $ do
            possibleMoves (Pos 'e' 5) (Piece White 130) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 1),(Move (Pos 'e' 5) (Pos 'e' 5) 2),(Move (Pos 'e' 5) (Pos 'e' 5) 3),(Move (Pos 'e' 5) (Pos 'e' 5) 4),(Move (Pos 'e' 5) (Pos 'e' 5) 5),(Move (Pos 'e' 5) (Pos 'e' 5) 6),(Move (Pos 'e' 5) (Pos 'e' 5) 7)] :: [Move])

        it "moves ProbeC" $ do
            possibleMoves (Pos 'e' 5) (Piece White 17) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 6) 0),(Move (Pos 'e' 5) (Pos 'e' 7) 0),(Move (Pos 'e' 5) (Pos 'e' 4) 0),(Move (Pos 'e' 5) (Pos 'e' 3) 0)] :: [Move])

        it "turns ProbeC " $ do
            possibleMoves (Pos 'e' 5) (Piece White 17) `shouldContain` ([(Move (Pos 'e' 5) (Pos 'e' 5) 1),(Move (Pos 'e' 5) (Pos 'e' 5) 2),(Move (Pos 'e' 5) (Pos 'e' 5) 3),(Move (Pos 'e' 5) (Pos 'e' 5) 5),(Move (Pos 'e' 5) (Pos 'e' 5) 6),(Move (Pos 'e' 5) (Pos 'e' 5) 7)] :: [Move])
        
        it "moves and turns LanceA " $ do
            possibleMoves (Pos 'e' 5) (Piece White 224) `shouldBe` ([(Move (Pos 'e' 5) (Pos 'd' 4) 0),(Move (Pos 'e' 5) (Pos 'c' 3) 0),(Move (Pos 'e' 5) (Pos 'b' 2) 0),(Move (Pos 'e' 5) (Pos 'd' 5) 0),(Move (Pos 'e' 5) (Pos 'c' 5) 0),(Move (Pos 'e' 5) (Pos 'b' 5) 0),(Move (Pos 'e' 5) (Pos 'd' 6) 0),(Move (Pos 'e' 5) (Pos 'c' 7) 0),(Move (Pos 'e' 5) (Pos 'b' 8) 0),(Move (Pos 'e' 5) (Pos 'e' 5) 1),(Move (Pos 'e' 5) (Pos 'e' 5) 2),(Move (Pos 'e' 5) (Pos 'e' 5) 3),(Move (Pos 'e' 5) (Pos 'e' 5) 4),(Move (Pos 'e' 5) (Pos 'e' 5) 5),(Move (Pos 'e' 5) (Pos 'e' 5) 6),(Move (Pos 'e' 5) (Pos 'e' 5) 7)] :: [Move])

        it "moves and turns LanceB " $ do
            possibleMoves (Pos 'e' 5) (Piece White 74) `shouldBe` ([(Move (Pos 'e' 5) (Pos 'f' 6) 0),(Move (Pos 'e' 5) (Pos 'g' 7) 0),(Move (Pos 'e' 5) (Pos 'h' 8) 0),(Move (Pos 'e' 5) (Pos 'f' 4) 0),(Move (Pos 'e' 5) (Pos 'g' 3) 0),(Move (Pos 'e' 5) (Pos 'h' 2) 0),(Move (Pos 'e' 5) (Pos 'd' 5) 0),(Move (Pos 'e' 5) (Pos 'c' 5) 0),(Move (Pos 'e' 5) (Pos 'b' 5) 0),(Move (Pos 'e' 5) (Pos 'e' 5) 1),(Move (Pos 'e' 5) (Pos 'e' 5) 2),(Move (Pos 'e' 5) (Pos 'e' 5) 3),(Move (Pos 'e' 5) (Pos 'e' 5) 4),(Move (Pos 'e' 5) (Pos 'e' 5) 5),(Move (Pos 'e' 5) (Pos 'e' 5) 6),(Move (Pos 'e' 5) (Pos 'e' 5) 7)] :: [Move])

        it "moves and turns LanceC " $ do
            possibleMoves (Pos 'e' 5) (Piece White 69) `shouldBe` ([(Move (Pos 'e' 5) (Pos 'e' 6) 0),(Move (Pos 'e' 5) (Pos 'e' 7) 0),(Move (Pos 'e' 5) (Pos 'e' 8) 0),(Move (Pos 'e' 5) (Pos 'f' 5) 0),(Move (Pos 'e' 5) (Pos 'g' 5) 0),(Move (Pos 'e' 5) (Pos 'h' 5) 0),(Move (Pos 'e' 5) (Pos 'd' 5) 0),(Move (Pos 'e' 5) (Pos 'c' 5) 0),(Move (Pos 'e' 5) (Pos 'b' 5) 0),(Move (Pos 'e' 5) (Pos 'e' 5) 1),(Move (Pos 'e' 5) (Pos 'e' 5) 2),(Move (Pos 'e' 5) (Pos 'e' 5) 3),(Move (Pos 'e' 5) (Pos 'e' 5) 4),(Move (Pos 'e' 5) (Pos 'e' 5) 5),(Move (Pos 'e' 5) (Pos 'e' 5) 6),(Move (Pos 'e' 5) (Pos 'e' 5) 7)] :: [Move])

        it "moves and turns Commander " $ do
            possibleMoves (Pos 'e' 5) (Piece White 170) `shouldBe` ([(Move (Pos 'e' 5) (Pos 'f' 6) 0),(Move (Pos 'e' 5) (Pos 'f' 4) 0),(Move (Pos 'e' 5) (Pos 'd' 4) 0),(Move (Pos 'e' 5) (Pos 'd' 6) 0),(Move (Pos 'e' 5) (Pos 'e' 5) 1),(Move (Pos 'e' 5) (Pos 'e' 5) 3),(Move (Pos 'e' 5) (Pos 'e' 5) 5),(Move (Pos 'e' 5) (Pos 'e' 5) 7)] :: [Move])
        

testListMoves :: Spec
testListMoves = describe "Module Game: listMoves ..." $ do
        it "game finished" $ do
            listMoves missingWCommanderBoard Black `shouldBe` ([] :: [Move])


        it "player white blocked pieces" $ do
            listMoves startBoard White `shouldNotContain` ([Move (Pos 'e' 8) (Pos 'e' 7) 5] :: [Move])


        it "player black blocked pieces" $ do
            listMoves startBoard Black `shouldNotContain` ([Move (Pos 'e' 1) (Pos 'f' 2) 5] :: [Move])

        

        
        

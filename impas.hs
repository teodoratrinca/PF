import System.Environment
import Data.List
import Data.Maybe
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


data IndexTop = A |B |C |D deriving Show
data Move = O   deriving Eq 
data Cell = Occupied Move | Empty | Occ IndexTop | Occc Int
data Player = Player1 |Player2 deriving Eq

instance Show Move where show O = "O" 

instance Show Cell where show (Occupied O) = "O"
                         show (Occc 1)     = "1"
                         show (Occc 4)     = "4"
                         show (Occc 3)     = "3"
                         show (Occc 2)     = "2"
                         show (Occ D)      = "D"
                         show (Occ C)      = "C"
                         show (Occ B)      = "B"
                         show (Occ A)      = "A"
                         show Empty        = " "

instance Show Player where show Player1 = " Player 1"
                           show Player2 = " Player 2"

instance Eq Cell where
  Occupied O == Occupied O = True
  Empty == Empty           = True
  _ == _                   = False

renderRow :: [Cell] -> String
renderRow row = intercalate " | " $ fmap show row

dividingLine :: String
dividingLine = "------------------"

--tabla initiala
-- doBoard[Empty, Empty,Empty,Empty,Empty,Empty,Empty,Empty,(Occ A),(Occupied O),Empty,Empty,(Occupied O),(Occupied O),Empty,Empty,Empty,Empty,Empty,Empty]
doBoard :: [Cell] -> IO ()
doBoard board = do
  putStrLn $ renderRow minusRow
  putStrLn dividingLine
  putStrLn $ renderRow zeroRow
  putStrLn dividingLine
  putStrLn $ renderRow firstRow
  putStrLn dividingLine
  putStrLn $ renderRow secondRow
  putStrLn dividingLine
  putStrLn $ renderRow thirdRow
 
  where minusRow=take 5 board
        zeroRow   = drop 5  .take 10 $ board
        firstRow  = drop 10  .take 15 $ board
        secondRow = drop 15 . take 20 $ board
        thirdRow  = drop 20  board

getBoardIndex :: String -> Int
getBoardIndex "A1" = 6
getBoardIndex "A2" = 7
getBoardIndex "A3" = 8
getBoardIndex "A4" = 9 
getBoardIndex "B1" = 11
getBoardIndex "B2" = 12
getBoardIndex "B3" = 13
getBoardIndex "B4" = 14
getBoardIndex "C1" = 16
getBoardIndex "C2" = 17
getBoardIndex "C3" = 18
getBoardIndex "C4" = 19
getBoardIndex "D1" = 21
getBoardIndex "D2" = 22
getBoardIndex "D3" = 23
getBoardIndex "D4" = 24
getBoardIndex _ = 30

--(>>=)            :: m a -> (a -> m b) -> m b

data CellTransform = Success [Cell] | Fail String [Cell] deriving (Show ,Eq)

verifyIsPieceHere :: [Cell]-> Int ->Maybe Int
verifyIsPieceHere board ix = if board !! ix == (Occupied O) then  Just ix else Nothing
 
verifyIsPieceThere :: [Cell] -> Int -> Maybe Int
verifyIsPieceThere board iy = if board !! iy == Empty then Just iy else Nothing

validateTheMove::[Cell]-> Int-> Int-> Maybe Int
validateTheMove board x y = if (verifyIsPieceHere board x)/=Nothing && (verifyIsPieceThere board y)/=Nothing then Just y else Nothing

validateForDelete::[Cell]-> Int-> Int-> Maybe Int
validateForDelete board x y = if (verifyIsPieceHere board x)/=Nothing && (verifyIsPieceThere board y)==Nothing then Just y else Nothing


moveTop :: Int -> Int -> Maybe Int
moveTop x y | x == 11 && y == 6 = Just 6 | x == 12 && y == 7 = Just 7 | x == 13 && y == 8 = Just 8
            | x == 14 && y == 9 = Just 9
            | x == 16 && y == 11 = Just 11
            | x == 17 && y == 12 = Just 12
            | x == 18 && y == 13 = Just 13
            | x == 19 && y == 14 = Just 14
            | x == 21 && y == 16 = Just 16
            | x == 22 && y == 17 = Just 17
            | x == 23 && y == 18 = Just 18
            | x == 24 && y == 19 = Just 19
            | True = Nothing

moveRight :: Int->Int-> Maybe Int
moveRight x y | x==6 && y==7 = Just 7
              | x==7 && y==8 = Just 8
              | x==8 && y==9 = Just 9
              | x==11 && y==12 = Just 12
              | x==12 && y==13 = Just 13
              | x==13 && y==14 = Just 14
              | x==16 && y==17 = Just 17
              | x==17 && y==18 = Just 18
              | x==18 && y==19 = Just 19
              | x==21 && y==22 = Just 22
              | x==22 && y==23 = Just 23
              | x==23 && y==24 = Just 24
              | True = Nothing


validatePlayer1::[Cell]-> Int-> Int-> Maybe Int
validatePlayer1 board x y = if (moveTop x y)/=Nothing && (validateTheMove board x y)/=Nothing then
                       Just y else Nothing
validatePlayer1ForDelete::[Cell]-> Int-> Int-> Maybe Int
validatePlayer1ForDelete board x y = if ((moveTop x y)/=Nothing && (validateForDelete board x y)/=Nothing) then
                       Just y else Nothing

validatePlayer1ForWin::[Cell]-> Int-> Int-> Maybe Int
validatePlayer1ForWin board x y = if ((moveTop x y)==Nothing && (validateTheMove board x y)==Nothing) then
                       Just y else Nothing

validatePlayer2::[Cell]-> Int-> Int-> Maybe Int
validatePlayer2 board x y = if (moveRight x y)/=Nothing && (validateTheMove board x y)/=Nothing then
                       Just y else Nothing
validatePlayer2ForDelete::[Cell]-> Int-> Int-> Maybe Int
validatePlayer2ForDelete board x y = if ((moveRight x y)/=Nothing && (validateForDelete board x y)/=Nothing) then
                       Just y else Nothing


deleteCell1:: String ->String ->[Cell]->CellTransform
deleteCell1 x y board = if( (validatePlayer1ForDelete board (getBoardIndex x) (getBoardIndex y))/=Nothing )
then  Success ((take (getBoardIndex x) board) ++ [Empty] ++ (drop (getBoardIndex x+1) board))
else  Fail "Miscarea nu poate fi executata" board
    
assignCell1 :: String ->String ->[Cell] -> CellTransform
assignCell1  x y board =if (validatePlayer1 board (getBoardIndex x) (getBoardIndex y))/=Nothing 
then  Success ((take (getBoardIndex y) board) ++ [Occupied O]++(drop (getBoardIndex y+1) board) )
else  Fail "Miscarea nu poate fi executata" board

deleteCell2:: String ->String ->[Cell]->CellTransform
deleteCell2 x y board = if( (validatePlayer2ForDelete board (getBoardIndex x) (getBoardIndex y))/=Nothing )
then  Success ((take (getBoardIndex x) board) ++ [Empty] ++ (drop (getBoardIndex x+1) board))
else  Fail "Miscarea nu poate fi executata" board
    
assignCell2 :: String ->String ->[Cell] -> CellTransform
assignCell2  x y board =if (validatePlayer2 board (getBoardIndex x) (getBoardIndex y))/=Nothing 
then  Success ((take (getBoardIndex y) board) ++ [Occupied O]++(drop (getBoardIndex y+1) board) )
else  Fail "Miscarea nu poate fi executata" board

playRound :: Player -> [Cell] -> IO()
playRound player board = do
  putStrLn $ (show player) ++ " muta acum"
  doBoard board
  putStr "\nPiesa pe care doresti sa o muti: "
  cell <- getLine
  putStr "\nLocul in care doresti sa muti piesa selectata: "
  cell2 <- getLine
  putStrLn $( show cell) ++ " ->" ++ ( show cell2)
  if (player==Player1 ) then
   case assignCell1 cell cell2 board of
    Fail err board -> do
      putStrLn err
      playRound player board
    Success newBoard -> 
	 case deleteCell1 cell cell2 newBoard of
      Fail err board -> do
       putStrLn err
       playRound player board
      Success newBoard2 -> do
        if (isThereAWinner player newBoard2)== True  then do
         putStrLn $ ("Playerul " ++ (show player) ++ " a castigat!")
         doBoard newBoard2
         return ()
        else playRound (nextMove player) newBoard2
  else
    case assignCell2 cell cell2 board of
    Fail err board -> do
      putStrLn err
      playRound player board
    Success newBoard -> 
	  case deleteCell2 cell cell2 newBoard of
      Fail err board -> do
       putStrLn err
       playRound player board
      Success newBoard2 -> do
        if (isThereAWinner player newBoard2)== True  then do
         putStrLn $ ("Playerul " ++ (show player) ++ " a castigat !")
         doBoard newBoard2
         return ()
        else playRound (nextMove player) newBoard2
	  
  
nextMove :: Player -> Player
nextMove player | player == Player2 = Player1
                | player == Player1 = Player2

indice :: [Cell] -> [Int]--imi returneaza indecsi unde sunt piesele pe tabla
indice = helper 0
    where helper _ [] = []
          helper i ((Occupied O):board) = i : helper (i+1) board
          helper i (_:board) = helper (i+1) board

--pentru fiecare index in functie de jucator vreau sa verific daca mai are mutari posibile

isThereAWinner :: Player ->[Cell]-> Bool
isThereAWinner player board  = if(length(indice board)>0) then
                                  if (player==Player2) then
								   if( (board !! (((indice board) !! 0)+1)==(Occupied O) || (moveRight ((indice board) !! 0) (((indice board) !! 0)+1)) == Nothing )
								     && (board !! (((indice board) !! 1)+1)==(Occupied O) || (moveRight ((indice board) !! 1) (((indice board) !! 1)+1)) == Nothing )
                                     && (board !! (((indice board)!! 2)+1)==(Occupied O) || (moveRight ((indice board) !! 2) (((indice board) !! 2)+1)) == Nothing )
                                     && (board !! (((indice board)!! 3)+1)==(Occupied O)|| (moveRight ((indice board) !! 3) (((indice board) !! 3)+1)) == Nothing ))then True 
					               else False
								  else
								   if ((board !! (((indice board) !! 0)-5)==(Occupied O)|| board !! (((indice board) !! 0)-5)==(Occc 1)|| board !! (((indice board) !! 0)-5)==(Occc 2)|| board !! (((indice board) !! 0)-5)==(Occc 3)|| board !! (((indice board) !! 0)-5)==(Occc 4))
								     && (board !! (((indice board) !! 1)-5)==(Occupied O)|| board !! (((indice board) !! 0)-5)==(Occc 1)|| board !! (((indice board) !! 0)-5)==(Occc 2)|| board !! (((indice board) !! 0)-5)==(Occc 3)|| board !! (((indice board) !! 0)-5)==(Occc 4)) 
                                     && (board !! (((indice board)!! 2)-5)==(Occupied O) || board !! (((indice board) !! 0)-5)==(Occc 1)|| board !! (((indice board) !! 0)-5)==(Occc 2)|| board !! (((indice board) !! 0)-5)==(Occc 3)|| board !! (((indice board) !! 0)-5)==(Occc 4))
                                     && (board !! (((indice board)!! 3)-5)==(Occupied O)|| board !! (((indice board) !! 0)-5)==(Occc 1)|| board !! (((indice board) !! 0)-5)==(Occc 2)|| board !! (((indice board) !! 0)-5)==(Occc 3)|| board !! (((indice board) !! 0)-5)==(Occc 4)))then True 
					               else False
								else False
 
  
main :: IO ()
main = do
  putStrLn $ "Jocul a inceput. Mult succes!"
  putStrLn $ "Jucatorul 1 are voie sa miste bila doar cate o casuta in sus"
  putStrLn $ "Jucatorul 2 are voie sa miste bila doar cate o casuta la dreapta"
  putStrLn $ "Pentru a iesi din joc puteti oricand sa apasati Ctrl + C"
  putStrLn $ "Formatul unei miscare este: mutam piesa de pe pozitia C2 pe pozitia C3"
  let newBoard =[Empty,(Occc 1),(Occc 2),(Occc 3),(Occc 4),(Occ A),Empty,Empty,Empty,Empty,(Occ B),Empty,Empty,Empty,Empty,(Occ C),(Occupied O),(Occupied O),Empty,Empty,(Occ D),(Occupied O),(Occupied O),Empty,Empty]
  playRound Player1 newBoard 

--test
--[Empty,(Occc 1),(Occc 2),(Occc 3),(Occc 4),(Occ A),Empty,Empty,(Occupied O),(Occupied O),Empty,Empty,Empty,(Occupied O),(Occupied O),(Occ C),Empty,Empty,Empty,Empty,(Occ D),Empty,Empty,Empty,Empty]


    





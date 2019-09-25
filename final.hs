module Main where

import Data.List
import Data.Char
import Control.Monad
--import Data.List.Utils

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Char

data Dir = DOWN | ACROSS deriving (Eq,Show)

type Puzzle = [Question]
type Layout = [String]
--[(Num of Question, Question, Hint, Answer)]
---type CrossWord = [(Int,String, String, String)]

data Question = Question { numQns :: Int, question :: String, qnsHint :: String, qnsAnswer :: String, qnsAnswered :: Bool, xVal :: Int, yVal :: Int, dir :: Dir
} deriving(Eq,Show)


myPuzzle = [ Question 1 "Fill in the 1 'ACROSS' in the FIRST ROW." "MONEY" "CENTS" False 0 0  ACROSS , Question 2 "Fill in the 2 'DOWN' in the FIRST ROW." "FRACTION" "NUMERATOR" False 0 2 DOWN,
 Question 3 "Fill in 3 'DOWN'." "FRACTION" "DENOMINATOR" False  1 10 DOWN , Question 4 "Fill in the 4 'ACROSS'." "DECIMAL" "HUNDREDTH" False 2 5  ACROSS, Question 5 "Fill in the 5 'DOWN' in the FIRST ROW." "BENJAMIN" "HUNDRED" False 2 5 DOWN,
 Question 6 "Fill in the 6 'ACROSS'." "Out Of 100" "PERCENT" False 4 0  ACROSS, Question 7 "Fill in the 7 'ACROSS'.---Enter 15 to quit" "FROM CENTIMETERS TO METERS" "CONVERSION" False 7 1 ACROSS, Question 8 "Fill in the 8 ACCROSS" "EARLIER HINTS" "FRACTION" False 10 4 ACROSS,
 Question 9 "Fill in the 9 'DOWN'." "FRACTION" "RATIO" False 10 5  DOWN, Question 10 "Fill in the 10 'ACROSS'." "CEILING OR FLOOR OF A NUMBER" "DECIMAL" False 13 2 ACROSS]

temp = [Question 1 "Fill in the 1 'ACROSS' in the FIRST ROW." "MONEY" "CENTS" False 0 0  ACROSS , Question 2 "Fill in the 2 'DOWN' in the FIRST ROW." "FRACTION" "NUMERATOR" False 0 2 DOWN,
 Question 3 "Fill in 3 'DOWN'." "FRACTION" "DENOMINATOR" False  1 10 DOWN, Question 4 "Fill in the 4 'ACROSS'." "DECIMAL" "HUNDREDTH" False 5 2  ACROSS, Question 5 "Fill in the 5 'DOWN' in the FIRST ROW." "BENJAMIN" "HUNDRED" False 2 5 DOWN,
 Question 6 "Fill in the 6 'ACROSS'." "Out Of 100" "PERCENT" False 0 4  ACROSS, Question 7 "Fill in the 7 'ACROSS'.---Enter 15 to quit" "FROM CENTIMETERS TO METERS" "CONVERSION" False 1 7 ACROSS, Question 8 "Fill in the 8 ACCROSS" "EARLIER HINTS" "FRACTION" False 4 10 ACROSS,
 Question 9 "Fill in the 9 'DOWN'." "FRACTION" "RATIO" False 10 5  DOWN, Question 10 "Fill in the 10 'ACROSS'." "CEILING OR FLOOR OF A NUMBER" "DECIMAL" False 2 13 ACROSS]

initPuzzle :: Layout
initPuzzle = [".....----------",
              "--.-------.----",
              "--.--.U......H-",
              "##.##.####.####",
              ".......#.....##",
              "##.##.#####.###",
              "##.##.#####.###",
              "#..........####",
              "##.##.####.####",
              "##########.####",
              "####.......N###",
              "#####.####.####",
              "#####.#########",
              "##.......######",
              "#####.#########"]


getDimension :: Puzzle -> Int
getDimension p = max largestTotalX largestTotalY
  where largestTotalX = maximum (map largestX p)
        largestTotalY = maximum (map largestY p)
        largestX q =
          case dir q of
            ACROSS -> (xVal q) + (length (qnsAnswer q))
            otherwise -> xVal q
        largestY q =
          case dir q of
            DOWN -> (yVal q) + (length (qnsAnswer q))
            otherwise -> yVal q

{-createCord::Puzzle->[(Int,Int,Char)]
createCord xs =
 [(xVal xs,(xVal xs) + 1,x) | x <- xs   ]-}
printingPuzzle::Layout->IO ()
printingPuzzle ls = (putStrLn . unlines) ls

addQuestion::Layout->Question -> Layout
addQuestion  ls q =
 case (dir q) of
  ACROSS -> updateThePuzzle q ls
  DOWN -> transpose(transpose(transpose(updateThePuzzle q (transpose ls))))

add ::Layout -> Question -> Layout
add ls q =
  if (dir q) == ACROSS then updateThePuzzle q ls
  else transpose(transpose(transpose(updateThePuzzle q (transpose ls))))

addingQuestionBlanks::[Question]->Layout
addingQuestionBlanks xs =
  ((foldl (addQuestion) (take 15 (blankLayout (15) (15))) xs))

updateThePuzzle::Question->Layout->Layout
updateThePuzzle qn ws =
 let (preY, l:postY) = splitAt (yVal qn) ws in
  let preX = take (xVal qn) l in
  let postX = drop ((xVal qn) + (length (qnsAnswer qn))) l in
  let spaces = if (qnsAnswered qn == True) then (qnsAnswer qn) else (replicate ((length (qnsAnswer qn))) '#') in

 preY ++ (preX++(spaces)++postX):postY

--Get the values to print
printHorizontalAcross::Int->String->Int->Int->[String]
printHorizontalAcross p ws colPos rightCol =
 if (p > 0 && rightCol >= colPos) then (ws:(printHorizontalAcross (p-1) ws colPos (rightCol+1))) else if (p > 0) then (printHorizontalAcross p ws colPos (rightCol+1)) else [""]

answerQns::Question->String
answerQns xs =
 if qnsAnswered xs == True then qnsAnswer xs else ""

--Takes in a value from printing a row
fillHorizontal::Int -> String
fillHorizontal p =
 if (p > 0) then "-"++(fillHorizontal (p-1)) else ""

--Takes a value to pass to fill Horizonta and a value to iter over rows
blankLayout::Int->Int->Layout
blankLayout val iter =
 if iter > 0 then ((fillHorizontal val)):(blankLayout val (iter - 1)) else [""]
 --if val > 0 then else [""]

replace::Int->a->[a]->[a]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

getQnsInfo::[Question]->Int->Question
getQnsInfo xs p =
 case xs of
  q:t -> if (numQns q) == p then q else getQnsInfo t p

--Update the Question to True
updateToTrueQn::Question->String->(Question,Bool)
updateToTrueQn qn line2 =
 if (line2 == qnsAnswer qn) then (qn { qnsAnswered = True}, True) else (qn,False)

--Change the letters to UpperCase
changeToUpper::String -> String
changeToUpper xs = [ toUpper x | x <- xs, isAlpha x ]

prompt::Int->[Question]->IO ()
prompt p xs = do
 putStrLn (question (getQnsInfo xs p) ++ "....." ++qnsHint (getQnsInfo xs p))
 line1 <- getLine

 let(qn,valbool) = (updateToTrueQn (getQnsInfo xs p) (changeToUpper line1)) in
  unless (valbool == True) $ do
   putStrLn("Wrong Word...Look at the HINT:")
   prompt p xs

keepGoing :: [Question] -> IO ()
keepGoing ps = do
 putStrLn ("Which number do you want to answer OR Enter the 15 to quit?")
 line <- getLine
 unless ((read line::Int) == 15) $ do
  prompt (read line::Int) ps
  printingPuzzle (addingQuestionBlanks (updateAtIndex (read line::Int) ps))

  keepGoing (updateAtIndex (read line::Int) ps)

--printPUZZLE:: Show a => myInitPuzzle -> IO ()
updateAtIndex::Int->[Question]->[Question]
updateAtIndex x xs = replace (x-1) qn' xs
 where
  qn' = (xs!!(x-1)) {qnsAnswered = True}

main :: IO ()
main = do
 putStrLn ("Solve This Puzzle!!!")
 --putStrLn ("Which number do you want to answer?")

 --line <- getLine
 printingPuzzle (addingQuestionBlanks temp)
 --printingPuzzle (blankLayout (getDimension myPuzzle) (getDimension myPuzzle))
 --printWithdots myPuzzle
 keepGoing temp --(read line::Int)

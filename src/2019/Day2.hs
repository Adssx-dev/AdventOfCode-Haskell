import System.IO
import Debug.Trace
import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Either

data Status = StatusDone [Int] | StatusRunning [Int]

main = do
    handle <- openFile "data/2019/Day2.txt" ReadMode
    contents <- hGetContents handle

    print $ part1 $ "1,9,10,3,2,3,11,0,99,30,40,50"
    --print $ part2 $ lines contents

    hClose handle

part1 :: [Char] -> Int
part1 inputStr = executeProgram program 0
    where
        program = map read (splitOn (==',') inputStr) :: [Int]


executeProgram :: [Int] -> Int -> Int
executeProgram program pointer = case programStatus of
        StatusDone p      -> trace (show (p, pointer)) $ head p
        StatusRunning p   -> trace (show (p, pointer)) executeProgram p (pointer + 4)
    where
        opcode = program !! pointer
        operand1 = program !! (program !! (pointer + 1))
        operand2 = program !! (program !! (pointer + 2))
        destination = program !! (pointer + 3)
        programStatus = trace (show (opcode, operand1, operand2, destination)) executeInstruction program opcode operand1 operand2 destination


executeInstruction :: [Int] -> Int -> Int -> Int -> Int -> Status
executeInstruction program 99 _ _ _ = StatusDone program
executeInstruction program 1 operand1 operand2 destination = StatusRunning $ replace program (destination, operand1 + operand2)
executeInstruction program 2 operand1 operand2 destination = StatusRunning $ replace program (destination, operand1 * operand2)

x :: Int -> [Int] -> Either [Int] [Int]
x 1 y = Left y
x 2 y = Right y

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p s =  case dropWhile p s of
                      [] -> []
                      s' -> w : splitOn p s''
                            where (w, s'') = break p s'


replace :: [Int] -> (Int, Int) -> [Int]
replace [] _ = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) =
  if n < 0
    then (x:xs)
    else x: replace xs (n-1,a)
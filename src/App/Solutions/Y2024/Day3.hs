module Solutions.Y2024.Day3
( part1
, part2
) where

import System.IO
import Debug.Trace
import Data.List
import Data.Char

data State = StateNone | -- No particular state
    StateM | -- Currently on letter "m"
    StateU | -- Currently on letter "u"
    StateL | -- Currently on letter "l"
    StateOB | -- Currently on opening bracket "("
    StateN1 Int Int | -- Currently reading first number (first value = id of current digit, second value = number read so far)
    StateCM Int | -- Currently on the comma between numbers
    StateN2 Int Int Int | -- Currently reading second number (first = id of current digit, second = value of first number before the comma, third = second number read so far)
    StateD | -- Currently on letter "d"
    StateO | -- Currently on letter "o"
    StateN | -- Currently on letter "n"
    StateAP -- Currently on apostrophe "'"
    -- There is no need for a state for the "t" or the closing bracket as we simply go back to "None" state

-- Used for part 2 : are instructions enabled ?
data Conditional = Enabled | Disabled 
    deriving (Eq)

part1 :: [Char] -> Maybe Int
part1 inputStr = Just $ parseProgramPt1 StateNone inputStr 0

part2 :: [Char] -> Maybe Int
part2 inputStr = Just $ parseProgramPt2 StateNone Enabled inputStr 0

-- Uses a state machine to represent the program.
-- Only uses the states from StateM to StateAP as "do" and "don't" are not needed yet
-- Each definition defines the possible transitions
parseProgramPt1 state [] accum = accum
parseProgramPt1 _ ('m':xs) accum = parseProgramPt1 StateM xs accum
parseProgramPt1 StateM ('u':xs) accum = parseProgramPt1 StateU xs accum
parseProgramPt1 StateU ('l':xs) accum = parseProgramPt1 StateL xs accum
parseProgramPt1 StateL ('(':xs) accum = parseProgramPt1 StateOB xs accum
parseProgramPt1 StateOB (x:xs) accum
    | isDigit x = parseProgramPt1 (StateN1 1 $ digitToInt x) xs accum
parseProgramPt1 (StateN1 digit value1) (x:xs) accum
    | isDigit x && digit < 3 = parseProgramPt1 (StateN1 (digit + 1) $ value1 * 10 + digitToInt x) xs accum
    | x == ',' = parseProgramPt1 (StateCM value1) xs accum
parseProgramPt1 (StateCM value1) (x:xs) accum
    | isDigit x = parseProgramPt1 (StateN2 1 value1 (digitToInt x)) xs accum
parseProgramPt1 (StateN2 digit value1 value2) (x:xs) accum
    | isDigit x && digit < 3 = parseProgramPt1 (StateN2 (digit + 1) value1 (value2 * 10 + digitToInt x)) xs accum
    | x == ')' = parseProgramPt1 StateNone xs ((value1 * value2)+accum)
parseProgramPt1 _ (x:xs) accum = parseProgramPt1 StateM xs accum

-- Uses a state machine to represent the program.
-- The second part is the same as first part, with only the distinction of adding or not the value depending on the current global state
parseProgramPt2 :: State -> Conditional-> [Char] -> Int -> Int
parseProgramPt2 state cond [] accum = accum
parseProgramPt2 _ cond ('d':xs) accum = parseProgramPt2 StateD cond xs accum
parseProgramPt2 StateD cond ('o':xs) accum = parseProgramPt2 StateO cond xs accum
parseProgramPt2 StateO cond ('n':xs) accum = parseProgramPt2 StateN cond xs accum
parseProgramPt2 StateO cond (_:xs) accum = parseProgramPt2 StateNone Enabled xs accum
parseProgramPt2 StateN cond ('\'':xs) accum = parseProgramPt2 StateAP cond xs accum
parseProgramPt2 StateAP cond ('t':xs) accum = parseProgramPt2 StateAP Disabled xs accum
parseProgramPt2 _ cond ('m':xs) accum = parseProgramPt2 StateM cond xs accum
parseProgramPt2 StateM cond ('u':xs) accum = parseProgramPt2 StateU cond xs accum
parseProgramPt2 StateU cond ('l':xs) accum = parseProgramPt2 StateL cond xs accum
parseProgramPt2 StateL cond ('(':xs) accum = parseProgramPt2 StateOB cond xs accum
parseProgramPt2 StateOB cond (x:xs) accum
    | isDigit x = parseProgramPt2 (StateN1 1 $ digitToInt x) cond xs accum
parseProgramPt2 (StateN1 digit value1) cond (x:xs) accum
    | isDigit x && digit < 3 = parseProgramPt2 (StateN1 (digit + 1) $ value1 * 10 + digitToInt x) cond xs accum
    | x == ',' = parseProgramPt2 (StateCM value1) cond xs accum
parseProgramPt2 (StateCM value1) cond (x:xs) accum
    | isDigit x = parseProgramPt2 (StateN2 1 value1 (digitToInt x)) cond xs accum
parseProgramPt2 (StateN2 digit value1 value2) cond (x:xs) accum
    | isDigit x && digit < 3 = parseProgramPt2 (StateN2 (digit + 1) value1 (value2 * 10 + digitToInt x)) cond xs accum
    | x == ')' = parseProgramPt2 StateNone cond xs (if cond == Enabled then (value1 * value2)+accum else accum )
parseProgramPt2 _ cond (x:xs) accum = parseProgramPt2 StateM cond xs accum
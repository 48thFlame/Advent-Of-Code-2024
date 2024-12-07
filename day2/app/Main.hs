module Main (main) where

import Data.Function ((&))
import Data.List as List
import Debug.Trace (traceShow)

debugLog :: (Show b) => b -> b
debugLog a = traceShow a a

isMonotonic :: (Int -> Int -> Bool) -> [Int] -> Bool
isMonotonic _ [] = True
isMonotonic _ [_] = True
isMonotonic cmp (a : b : xs) = cmp a b && isMonotonic cmp (b : xs)

isIncreasing :: [Int] -> Bool
isIncreasing = isMonotonic (<)

isDecreasing :: [Int] -> Bool
isDecreasing = isMonotonic (>)

{- |
no need to check if bigger then 0,
because checking if is strictly decreasing/increasing
-}
safeDist :: Int -> Int -> Bool
safeDist a b =
    abs (b - a) <= 3

isSafeDist :: [Int] -> Bool
isSafeDist = isMonotonic safeDist

isSafeReport :: [Int] -> Bool
isSafeReport l =
    (isDecreasing l || isIncreasing l) && isSafeDist l

lstStrToInt :: [String] -> [Int]
lstStrToInt = map read

part1 :: IO ()
part1 = do
    rawInput <- readFile "input.txt"
    let output =
            lines rawInput
                & map (isSafeReport . lstStrToInt . words)
                & List.filter id
                & List.length
    print output

isFixableReport :: [Int] -> Bool
isFixableReport l =
    let oneLengthLess = List.length l - 1
     in List.subsequences l
            & List.filter (\sl -> List.length sl == oneLengthLess)
            & List.any isSafeReport

part2 :: IO ()
part2 = do
    rawInput <- readFile "input.txt"
    let reports =
            lines rawInput
                & map (lstStrToInt . words)

    let pureSafe =
            reports
                & List.filter isSafeReport
                & List.length

    let maybeUnsafe =
            reports
                & List.filter (not . isSafeReport)
                & List.filter isFixableReport
                & List.length

    print (pureSafe + maybeUnsafe)

main :: IO ()
main = do
    -- part1
    part2

-- let a = [1, 2, 3, 5, 7] & List.reverse
-- print $ isIncreasing a
-- print $ isDecreasing a
-- print $ isSafeDist a
-- print $ isSafeReport a

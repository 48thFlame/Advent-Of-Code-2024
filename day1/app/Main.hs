module Main (main) where

import Data.Function ((&))
import Data.List as List

-- import System.IO

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

zipFromTuple :: ([a], [b]) -> [(a, b)]
zipFromTuple (x, y) = zip x y

tupleStringsToInts :: (String, String) -> (Int, Int)
tupleStringsToInts (a, b) = (read a, read b)

getDis :: (Int, Int) -> Int
getDis (x, y) = abs $ y - x

part1 :: IO ()
part1 = do
    rawInput <- readFile "input.txt"
    let output =
            lines rawInput
                & map (tupleStringsToInts . tuplify2 . words)
                & unzip
                & mapTuple List.sort
                & zipFromTuple
                & map getDis
                & sum
    -- & map read :: [Int]
    print $ output

countOccurrences :: Int -> [Int] -> Int
countOccurrences x = length . filter (== x)

countAndMultiply :: [Int] -> Int -> Int
countAndMultiply lst x =
    x * countOccurrences x lst

part2 :: IO ()
part2 = do
    rawInput <- readFile "input.txt"
    let (l1, l2) =
            lines rawInput
                & map (tupleStringsToInts . tuplify2 . words)
                & unzip

    let output =
            map (countAndMultiply l2) l1
                & sum

    -- & mapTuple List.sort
    -- & zipFromTuple
    -- & map getDis
    -- & map read :: [Int]
    print $ output

main :: IO ()
main = do
    part1
    part2
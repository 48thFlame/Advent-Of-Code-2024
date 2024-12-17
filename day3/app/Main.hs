module Main (main) where

import Data.Function ((&))

import Data.List (foldl')
import Data.List qualified as List
import Debug.Trace (traceShow)
import Text.Regex.TDFA

debugLog :: (Show b) => b -> b
debugLog a =
    traceShow a a

getALlMul :: String -> [String]
getALlMul inp =
    getAllTextMatches (inp =~ "mul\\([0-9]+,[0-9]+\\)")

splitAtComma :: String -> (Int, Int)
splitAtComma s =
    -- let (a, _, b) = s =~ "," :: (String, String, String)
    --  in (read a, read b)
    let (a, b) = sl ',' s
     in (read a, read b)

parseMulAndGetAnswer :: String -> Int
parseMulAndGetAnswer mul =
    -- List.take
    List.drop 4 mul
        & List.reverse
        & List.drop 1
        & List.reverse
        -- look like "n,n" in a string
        & splitAtComma
        & uncurry (*)

part1 :: IO ()
part1 = do
    rawInput <- readFile "input.txt"
    -- let rawInput = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    -- let rawInput = "mul ( 2 , 4 )mul(4*mul(6,9!?(12,34)mul(200,2)"
    -- let rawInput = "mul\([0-9]+,[0-9]+\)|do\(\)|don't\(\)"
    let output =
            rawInput
                & getALlMul
                & map parseMulAndGetAnswer
                & sum
    -- rawInput
    print output

getAllInstructions :: String -> [String]
getAllInstructions s =
    getAllTextMatches (s =~ expression)
  where
    expression = "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)"

splitList :: (Eq a) => [a] -> a -> [a] -> ([a], [a])
-- splitAt l value acc =
splitList [] _ acc = (acc, []) -- if doesn't find return entire list
splitList (x : xs) s acc =
    if x == s
        then
            (acc, xs)
        else
            splitList xs s (acc ++ [x])

sl :: (Eq a) => a -> [a] -> ([a], [a])
sl s l =
    splitList l s []

goThrewInstructions :: Int -> Bool -> [String] -> Int
goThrewInstructions n _ [] = n
goThrewInstructions n inDo (x : xs) =
    if inDo
        then case x of
            "do()" ->
                goThrewInstructions n True xs
            "don't()" ->
                goThrewInstructions n False xs
            _ ->
                goThrewInstructions (n + parseMulAndGetAnswer x) True xs
        else case x of
            "do()" ->
                goThrewInstructions n True xs
            _ ->
                goThrewInstructions n False xs

part2 :: IO ()
part2 = do
    rawInput <- readFile "input.txt"
    -- let rawInput = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
    -- let rawInput = "mul ( 2 , 4 )mul(4*mul(6,9!?(12,34)mul(200,2)"
    -- let rawInput = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
    let output =
            rawInput
                & getAllInstructions
                & goThrewInstructions 0 True
    print output

main :: IO ()
main = do
    part1
    part2

module Main where

import Control.Monad
import Data.List.Split
import Data.Maybe
import Text.Printf
import Text.Regex

simpleArraySum :: IO ()
simpleArraySum = do
    n <- getLine
    arr <- getLine
    let arrAsInts = map read (splitOn " " arr) :: [Int]
    putStrLn $ show $ sum arrAsInts

compareTheTriplets_diff :: [(Int,Int)] -> (Int,Int)
compareTheTriplets_diff ziparr = (length $ filter (\(a,b) -> a > b) ziparr, length $ filter (\(a,b) -> b > a) ziparr)

compareTheTriplets :: IO ()
compareTheTriplets = do
    alice <- getLine
    bob <- getLine

    let aliceInts = map read (splitOn " " alice) :: [Int]
    let bobInts = map read (splitOn " " bob) :: [Int]
    let scores = compareTheTriplets_diff $ zip aliceInts bobInts
    putStrLn $ printf "%d %d" (fst scores) (snd scores)

bigSum :: IO ()
bigSum = do
    n <- getLine
    arr <- getLine

    let arrBigInts = map read (splitOn " " arr) :: [Integer]
    putStrLn $ show $ sum arrBigInts

diagonalDifference_leftDiagSum :: Int -> Int -> [[Int]] -> Int
diagonalDifference_leftDiagSum _ _ [] = 0
diagonalDifference_leftDiagSum n i (x:xs) = x!!i + (diagonalDifference_leftDiagSum n (i+1) xs)

diagonalDifference_rightDiagSum :: Int -> Int -> [[Int]] -> Int
diagonalDifference_rightDiagSum _ _ [] = 0
diagonalDifference_rightDiagSum n i (x:xs) = x!!(n-i-1) + (diagonalDifference_rightDiagSum n (i+1) xs)

diagonalDifference :: IO ()
diagonalDifference = do
    n <- getLine
    matrixLines <- replicateM (read n) getLine
    let matrixInts = map (\row -> map read (splitOn " " row)) matrixLines :: [[Int]]
    let leftDiag = diagonalDifference_leftDiagSum (read n) 0 matrixInts
    let rightDiag = diagonalDifference_rightDiagSum (read n) 0 matrixInts
    putStrLn $ show $ abs (leftDiag - rightDiag)

plusMinus :: IO ()
plusMinus = do
    n <- getLine
    arr <- getLine
    let arrInts = map read (splitOn " " arr) :: [Int]
    let npos = length $ filter ((<) 0) arrInts
    let nzero = length $ filter ((==) 0) arrInts
    let nneg = length $ filter ((>) 0) arrInts
    let npos_rat = (realToFrac npos) / (read n :: Float)
    let nzero_rat = (realToFrac nzero) / (read n :: Float)
    let nneg_rat = (realToFrac nneg) / (read n :: Float)
    putStrLn $ printf "%.6f" npos_rat
    putStrLn $ printf "%.6f" nneg_rat
    putStrLn $ printf "%.6f" nzero_rat

staircase_make :: Int -> Int -> [String]
staircase_make (-1) _ = []
staircase_make i n = ((replicate i ' ') ++ (replicate (n-i) '#')) : staircase_make (i-1) n

staircase :: IO ()
staircase = do
    n <- getLine
    sequence_ $ map (\x -> putStrLn x) (staircase_make ((read n)-1) (read n))
    

minMaxSum :: IO ()
minMaxSum = do
    arr <- getLine
    let arrInts = map read (splitOn " " arr) :: [Int]
    putStrLn $ printf "%d %d" ((sum arrInts) - (maximum arrInts)) ((sum arrInts) - (minimum arrInts))

birthdayCandles :: IO ()
birthdayCandles = do
    n <- getLine
    arr <- getLine
    let arrInts = map read (splitOn " " arr) :: [Int]
    putStrLn $ show $ length $ filter (\x -> x == maximum arrInts) arrInts

timeConversion_convert :: String -> Maybe [String] -> String
timeConversion_convert a Nothing = take 8 a
timeConversion_convert _ x = let comps = fromJust x in (show (12 + (read $ head comps :: Int))) ++ comps!!1

timeConversion :: IO ()
timeConversion = do
    twelveTime <- getLine
    let pmRegex = mkRegex "^([0-9]{2}):([0-9]{2}:[0-9]{2})PM$"
    let pmMatch = matchRegex pmRegex twelveTime
    putStrLn $ timeConversion_convert twelveTime pmMatch

main :: IO ()
main = timeConversion
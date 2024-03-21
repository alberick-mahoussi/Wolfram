{-
-- EPITECH PROJECT, 2023
-- wolfram
-- File description:
-- Lines_mngments
-}

module Lines_mngments (createFisrtline, printChars) where

import  System.IO
import System.Exit (exitSuccess)

createFisrtline::Int->[Char]
createFisrtline window 
    | window `mod` 2 == 0 = replicate n ' ' ++ "*" ++ replicate (n - 1) ' '
    | otherwise = (replicate n ' ' ++ "*" ++ replicate n ' ')
        where n = window `div` 2


printChars :: [Char]->Int->Int->Int->Int->Int->IO()
printChars line nb_gen start win nb_ln mov
    | nb_gen >= start && nb_gen < (nb_ln + start) = checkTypeMov win nb_gen mov line
    | nb_gen >= start && nb_ln == -1 = checkTypeMov win nb_gen mov line
    | nb_gen < start = putStr ""
    | otherwise = exitSuccess                 

checkTypeMov::Int->Int->Int->[Char]->IO()
checkTypeMov win num x line | x >= 0 = leftShift win num x line
                 | otherwise = rightShift win num (x * (-1)) line

leftShift::Int->Int->Int->[Char] -> IO()
leftShift win num shiftAmount x =
    putStr (replicate shiftAmount ' ')
    >> putStrLn (take win (drop num x))

rightShift:: Int->Int->Int->[Char] -> IO()
rightShift win num shiftAmount x = 
    putStr (replicate (shiftAmount - length x) ' ')
    >> putStrLn (take win (drop num x))

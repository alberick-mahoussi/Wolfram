{-
-- EPITECH PROJECT, 2023
-- wolfram
-- File description:
-- the puzzle designer in haskell
-}

module Main (main) where

import System.Environment
import System.IO
import System.Exit

import Lib(usage)
import ChegArgs

import Rules

errorMessage :: String -> IO ()
errorMessage str = putStr  usage
                    >> putStr ("\nERROR: " ++ str)
                    >> exitWith (ExitFailure 84)

rigor :: [String] -> IO ()
rigor []  = errorMessage "minimum 2 params for the programms"
rigor [x] = errorMessage "the parameter not have rule value or invalid program"
rigor x   | head x /= "--rule" = errorMessage "the first parameters isn't rule"
          | odd (length x) = errorMessage "not good number of arguments"
          | otherwise = return ()

checkRulesOption :: Int -> IO()
checkRulesOption 30 = return ()
checkRulesOption 90 = return ()
checkRulesOption 110 = return ()
checkRulesOption _ = putStrLn "This option is not availaible"
                    >> exitSuccess

checkStringWolfram :: [String] -> IO()
checkStringWolfram [] = return ()
checkStringWolfram["error"] =
    errorMessage "invalid value or type in arguments"
checkStringWolfram(_:xs) = checkStringWolfram xs
checkStringWolfram("error":xs) = errorMessage "invalid value in arguments"

checkInfWolfram :: Int->Int->Int->Int->Int-> IO()
checkInfWolfram rule start lines window mov
    |   start < 0 = errorMessage "Bad Start Value"
    |   lines < 0 && lines /= -1 = errorMessage "Bad Lines Value"
    |   window < 0 = errorMessage "Bad Value of window"
    |rule `elem` [30, 90, 110] = startWolfram rule start lines window mov
checkInfWolfram _ _ _ _ _ = errorMessage "Bad Params"


startWolfram :: Int->Int->Int->Int->Int-> IO()
startWolfram 30 start line window mov =
    genWolfram "   **** "start line window mov
startWolfram 90 start line window mov =
    genWolfram " * ** * "start line window mov
startWolfram 110 start line window mov = 
    genWolfram " ** *** "start line window mov

main :: IO()
main = do
    av <-getArgs
    rigor av
    let argsvalue = checkTypeArguments av defaultOpt
    checkStringWolfram argsvalue
    checkRulesOption ((read :: String ->Int) (head argsvalue))
    let rule = (read :: String ->Int) (head argsvalue)
    let start = (read :: String -> Int) (argsvalue !! 1)
    let line = (read :: String -> Int) (argsvalue !! 2)
    let window = (read :: String -> Int) (argsvalue !! 3)
    let mov = (read :: String -> Int) (argsvalue !! 4)
    checkInfWolfram rule start line window mov
    exitSuccess


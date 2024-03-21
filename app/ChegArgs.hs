{-
-- EPITECH PROJECT, 2023
-- wolfram
-- File description:
-- ChegArgs
-}

module ChegArgs (
    checkTypeArguments,
    defaultOpt
    ) where
    
import Text.Read(readMaybe)

import Data.Char

defaultOpt::[String]
defaultOpt = ["0", "0", "-1", "80", "0"]

isInteger::String->Bool
isInteger s = case reads s :: [(Integer,String)] of
                [(_, "")] -> True
                _         -> False
isInteger _ = False


isIntegerNegative::String->Bool
isIntegerNegative [xc] = isInteger [xc]
isIntegerNegative ('-':xs) = isInteger xs
isIntegerNegative x = isInteger x


programVal ::Int -> String -> [String] -> [String]
programVal index newStr list = 
    take index list ++ [newStr] ++ drop (index+1) list

checkTypeArguments :: [String] -> [String] -> [String]
checkTypeArguments ("--rule":x:xs) def_t 
    | isInteger x = checkTypeArguments xs (programVal 0 x def_t)
    | otherwise = checkTypeArguments xs (programVal 0 "error" def_t)
checkTypeArguments ("--start":x:xs) def_t
    | isInteger x = checkTypeArguments xs (programVal 1 x def_t)
    | otherwise = checkTypeArguments xs (programVal 1 "error" def_t)
checkTypeArguments ("--lines":x:xs) def_t
    | isInteger x = checkTypeArguments xs (programVal 2 x def_t)
    | otherwise = checkTypeArguments xs (programVal 2 "error" def_t)
checkTypeArguments ("--window":x:xs) def_t 
    | isInteger x = checkTypeArguments xs (programVal 3 x def_t)
    | otherwise = checkTypeArguments xs (programVal 3 "error" def_t)
checkTypeArguments ("--move":x:xs) def_t   
    | isIntegerNegative x = checkTypeArguments xs (programVal 4 x def_t)
    | otherwise = checkTypeArguments xs (programVal 4 "error" def_t)
checkTypeArguments (_:v:xs) def_t = ["error"]
checkTypeArguments _ def_t = def_t


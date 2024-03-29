{-
-- EPITECH PROJECT, 2023
-- wolfram
-- File description:
-- the puzzle designer in haskell
-}

module Lib
    ( usage
    ) where

usage ::[Char]
usage =  "USAGE:\n\t./wolfram --rule value [--start value] [--lines value]\
         \ [--window value] [--move value]\n\n--rule\t:\tselect the rule to\
         \ apply\n--start\t:\tthe first line of the generation to display\n\
         \--lines\t:\tthe number of lines to display\n--window:\tthe size\
         \of the display window\n--move\t:\ttranslation to apply to the\
         \window (right if positive, left if negative)"
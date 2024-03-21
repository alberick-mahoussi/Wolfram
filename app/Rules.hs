{-
-- EPITECH PROJECT, 2023
-- woalfram
-- File description:
-- Rules
-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Rules (genWolfram) where

import Lines_mngments(createFisrtline, printChars)

getCharacter::[Char]->[Char]->Char
getCharacter rule [' ', ' ', ' '] = rule !! 7
getCharacter rule [' ', ' ', '*'] = rule !! 6
getCharacter rule [' ', '*', ' '] = rule !! 5
getCharacter rule [' ', '*', '*'] = rule !! 4
getCharacter rule ['*', ' ', ' '] = rule !! 3
getCharacter rule ['*', ' ', '*'] = rule !! 2
getCharacter rule ['*', '*', ' '] = rule !! 1
getCharacter rule ['*', '*', '*'] = rule !! 0
getCharacter _ _ = ' '


getNxtLine::[Char]->[Char]->[Char]
getNxtLine rule (left:center:right:xs) = 
      getCharacter rule [left, center, right]:getNxtLine rule (center:right:xs)
getNxtLine _ [_, ' '] = [' ']
--getNxtLine _ [_, '*'] = ['*']
getNxtLine _ _ = []


loop::[Char]->[Char]->Int->Int->Int->Int->Int->IO()
loop (z:xs) bin start lin window mov x = 
      printChars n_line (x+1) start window lin mov
                        >> loop n_line bin start lin window mov (x+1)
                        where n_line = z:getNxtLine bin ((' ':z:xs) ++ " ")

genWolfram::[Char]->Int->Int->Int->Int->IO()
genWolfram bin start lin window mov = printChars first x start window lin mov
            >> loop first bin start lin window mov x
                where first = createFisrtline window
                      x = 0
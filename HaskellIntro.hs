{-# OPTIONS_GHC -fwarn-tabs #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant where" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module HaskellIntro where

import Set
import Distribution.Simple.Utils (xargs)

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit = if x < 10 then x else mod x 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = if x < 10 then 0 else div x 10

toDigits :: Integer -> [Integer]
toDigits x = if dropLastDigit x == 0 then [] else map lastDigit (toDigits (dropLastDigit x))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = doubleHelp (reverse x)

doubleHelp :: [Integer] -> [Integer]
doubleHelp [] = []
doubleHelp (x:xs) = x : (2 * head xs) : doubleHelp (tail xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate 0 = False
validate x = sumDigits (toDigits x) <= 4012888888881881
--
-- Problem 2
--
pow2 :: (Eq t1, Num t1) => t2 -> t3 -> t1 -> t2
pow2 f x n = pow2 f x n
    where
        pow2 f x 1 = f
        pow2 f x n = pow2 f x (n-1)

g :: Integer -> Integer
g x = g x
    where
        g 0 = 0
        g x = x - pow2 (g (x-1) 2)

h :: Integer -> Integer

h x = h x
    where
        h 0 = 0
        h x = x - pow2 (h (x-1) 3)

d :: Int -> Integer -> Integer
d x = d x
    where
        d 0 = 0
        d x = x - pow2 (d (x-1) 1)

--
-- Problem 3
--
--powerSet :: Set a -> Set(Set a)
--powerSet x = insert empty (step isEmpty pxs) where
--  step x pxs = insert (singleton pxs) 
--                insert (mapSet split pxs )  q\



square :: Int -> Int
square x = x*x



powerSet :: Set a -> Set (Set a)
powerSet isEmpty = empty
powerSet x = powerSHelp (split x)

powerSHelp :: (a, Set a) -> Set (Set a)
powerSHelp (x, xs) = mapSet (insert x) (powerSet xs)  `union` powerSet xs



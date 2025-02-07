{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task1 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (reverse, map, filter, sum, foldl, foldr, length, head, tail, init, last, show, read)


-----------------------------------
-- 
-- Returns all elements of a list except last element
--
-- Usage example: 
-- 
-- >>> exceptLast [1, 2, 3]
-- [1,2]
-- >>> exceptLast [1]
-- []
-- >>> exceptLast [3, 4, 5, 6, 7]
-- [3,4,5,6]

exceptLast :: [Int] -> [Int]
exceptLast []     = []
exceptLast [_]    = []
exceptLast (x:xs) = x:exceptLast xs 
-----------------------------------
-- 
-- Gets the length of a list of Ints
--
-- Usage example:
--
-- >>> length [1, 2, 3]
-- 3
-- >>> length [5]
-- 1
-- >>> length []
-- 0


length :: [Int] -> Int
length [] = 0
length (_:xs) = 1 + length xs  

-----------------------------------
-- 
-- Gets the last element of a list
--
-- Usage example: 
-- 
-- >>> tail [1, 2, 3]
-- 3
-- >>> tail [1] 
-- 1
-- >>> tail [5, 9, 7]
-- 7
tail :: [Int] -> Int
tail []     = error "Empty list has no tail"
tail [x]    = x 
tail (_:xs) = tail xs

-----------------------------------
--
-- Checks whether the last digit is a valid check digit
-- for the rest of the given number using Luhn algorithm
--
-- Usage example:
--
-- >>> validate 3456
-- False
-- >>> validate 34561
-- True
-- >>> validate 34562
-- False




validate :: Integer -> Bool
validate x = luhn (exceptLast (toDigits x)) == tail (toDigits x)

-----------------------------------
-- Helper function to compute (10 - (s mod 10)) mod 10
--
-- Usage example:
--
-- >>> luhnFunc 19
-- 1
luhnFunc :: Int -> Int
luhnFunc s = (10 - (s `mod` 10)) `mod` 10


-----------------------------------
--
-- Computes check digit for given digits using Luhn algorithm
--
-- Usage example:
--
-- >>> luhn [3,4,5,6]
-- 1


luhn :: [Int] -> Int
luhn x = luhnFunc (sum (map normalize (doubleEveryOther (reverse x))))

-----------------------------------
--
-- Produces list of digits for given positive number;
-- otherwise (for zero and negative numbers) returns empty BlockedIndefinitelyOnSTM
--  
-- Usage example:
--
-- >>> toDigits 3456
-- [3,4,5,6]
-- >>> toDigits 0
-- [0]
-- >>> toDigits (-123)
-- []

toDigits :: Integer -> [Int]
toDigits n
    | n < 0     = []
    | n < 10    = [fromIntegral n]
    | otherwise = toDigits (n `div` 10) ++ toDigits (n `mod` 10)



-----------------------------------
--
-- Produces list in reverse order to the given one
--
-- Usage example:
--
-- >>> reverse "Hello"
-- "olleH"
-- >>> reverse [3,4,5,6]
-- [6,5,4,3]

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

-----------------------------------
--
-- Doubles every other digit starting from first one
--
-- Usage example:
--
-- >>> doubleEveryOther [6,5,4,3]
-- [12,5,8,3]

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther [x] = [2 * x]
doubleEveryOther (x:y:xs) = 2 * x:y:doubleEveryOther xs

-----------------------------------
--
-- Normalizes given number to single digit by subtracting 9
-- if it is greater than or equal to 10
--
-- (Assumes inputs between 0 and 18)
--
-- Usage example:
--
-- >>> normalize 12
-- 3
-- >>> normalize 1
-- 1

normalize :: Int -> Int
normalize x
    | x >= 10    = x - 9
    | otherwise  = x

-----------------------------------
--
-- Produces list with given function applied to each element
-- in given list
--
-- Usage example:
--
-- >>> map (\x -> x * 2) [1,2,3,4]
-- [2,4,6,8]

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x:map f xs

-----------------------------------
--
-- Computes sum of given list of numbers
--
-- Usage example:
--
-- >>> sum [3,8,5,3]
-- 19
-- >>> sum []
-- 0

sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs

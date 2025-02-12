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
-- >>> init [1, 2, 3]
-- [1,2]
-- >>> init [1]
-- []
-- >>> init [3, 4, 5, 6, 7]
-- [3,4,5,6]

init :: [a] -> [a]
init []     = []
init [_]    = []
init (x:xs) = x : init xs 
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
-- >>> last [1, 2, 3]
-- 3
-- >>> last [1] 
-- 1
-- >>> last [5, 9, 7]
-- 7
-- >>> last "abc"
-- 'c'
last :: [a] -> a
last []     = error "Empty list has no tail"
last [x]    = x 
last (_:xs) = last xs

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
validate x = luhn (init (toDigits x)) == last (toDigits x)

-----------------------------------
-- Helper function to compute (n - (s mod n)) mod n
--
-- Usage example:
--
-- >>> luhnFunc 10 19
-- 1
luhnFunc :: Int -> Int -> Int
luhnFunc n s = (n - (s `mod` n)) `mod` n


-----------------------------------
--
-- Computes check digit for given digits using Luhn algorithm
--
-- Usage example:
--
-- >>> luhn [3,4,5,6]
-- 1


luhn :: [Int] -> Int
luhn x = luhnFunc 10 (sum (map (normalize 10) (doubleEveryOther (reverse x))))

-----------------------------------
--
-- Produces list of digits for given positive number;
-- otherwise (for negative numbers) returns empty list
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
doubleEveryOther (x:y:xs) = 2 * x : y : doubleEveryOther xs

-----------------------------------
--
-- Normalizes given number to single digit by subtracting N - 1
-- if it is greater than or equal to N
--
-- (Assumes inputs between 0 and 2 * N)
--
-- Usage example:
--
-- >>> normalize 10 12
-- 3
-- >>> normalize 10 1
-- 1
-- >>> normalize 16 17
-- 2

normalize :: Int -> Int -> Int 
normalize n x
    | x >= n    = x - n + 1
    | otherwise = x


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
map f (x:xs) = f x : map f xs

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

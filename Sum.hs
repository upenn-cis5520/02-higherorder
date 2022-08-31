{-
---
fulltitle: foldr vs. foldl
---

This module contains some quick examples demonstrating the difference between
foldr and foldl. It is advanced material for CIS 5520.
-}

module Sum where

import Prelude hiding (foldl, foldr)

{-
Start with a concrete example of a fold --- the "sum" function that adds
 together all numbers in a list.
-}

sum1 :: [Int] -> Int
sum1 [] = 0
sum1 (x : xs) = x + sum1 xs

sum2 :: [Int] -> Int
sum2 [] = 0
sum2 (x : xs) = sum2 xs + x

sum3 :: [Int] -> Int
sum3 = sumAux 0
  where
    sumAux acc [] = acc
    sumAux acc (x : xs) = sumAux (acc + x) xs

sum4 :: [Int] -> Int
sum4 = sumAux 0
  where
    sumAux acc [] = acc
    sumAux acc (x : xs) = sumAux (x + acc) xs

{-
All of these functions give us the same result because (+) is associative and commutative. But none give us exactly the same computation. But they don't evaluate in the same way.

         sum1 [1,2,3]
           == 1 + (2 + (3 + 0))
         sum2 [1,2,3]
           == ((0 + 3) + 2) + 1
         sum3 [1,2,3]
           == ((0 + 1) + 2) + 3
         sum4 [1,2,3]
           == (3 + (2 + (1 + 0)))

And, none of them are tail recursive. To get an actual tail recursive function
in Haskell, we need to evaluate the accumulator before the recursive call.
The operation `($!)` is CBV function application, i.e. it forces GHC to
evaluate the argument before making a recursiove call.
-}

sum5 :: [Int] -> Int
sum5 = sumAux 0
  where
    sumAux acc [] = acc
    sumAux acc (x : xs) = (sumAux $! (x + acc)) xs

{-
We can generalize the examples above to create several different
recursion patterns over lists.
-}

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b = go
  where
    go [] = b
    go (x : xs) = x `f` go xs

foldrFlip :: (b -> a -> b) -> b -> [a] -> b
foldrFlip f b = go
  where
    go [] = b
    go (x : xs) = go xs `f` x

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f = go
  where
    go acc [] = acc
    go acc (x : xs) = go (acc `f` x) xs

foldlFlip :: (a -> b -> b) -> b -> [a] -> b
foldlFlip f = go
  where
    go acc [] = acc
    go acc (x : xs) = go (x `f` acc) xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f = go
  where
    go acc [] = acc
    go acc (x : xs) = (go $! acc `f` x) xs

{-
--------------------------------------------------

Here are some micro-benchmarks for thinking about laziness and saved
computation.
-}

-- A potentially big computation
h1 :: Int -> Int
h1 y = sum1 [1 .. y * 100000]

-- A potentially big computation (ignore first argument)
f1 :: Int -> Int -> Int
f1 _x y = sum1 [1 .. y * 100000]

-- A potentially big computation (don't ignore first argument)
g1 :: Int -> Int -> Int
g1 x y = if x > 0 then sum1 [1 .. y * 100000] else sum2 [1 .. y * 100000]

-- Call h1 with same argument multiple times.
u1 :: Int
u1 = h1 1 + h1 1 + h1 1

-- Call f1 with same and different arguments multiple times.
v1 :: Int
v1 = f1 0 1 + f1 1 1 + f1 2 1

v2 :: Int
v2 = f1 0 1 + f1 0 1 + f1 0 1

-- Call g1 with same and different arguments multiple times.
w1 :: Int
w1 = g1 0 1 + g1 1 1 + g1 2 1

w2 :: Int
w2 = g1 0 1 + g1 0 1 + g1 0 1

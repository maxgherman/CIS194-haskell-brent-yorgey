    module Fibonacci where

        fib :: Integer -> Integer
        fib x
            | x <= 0 = 0
            | x == 1 = 1
            | otherwise = fib (x-1) + fib (x-2)

        fibs1 :: [Integer]
        fibs1 = [ fib x | x <- [0..]]

        fibs2 :: [Integer]
        fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

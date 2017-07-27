module Sieve where

    sieveSundaram :: Integer -> [Integer]
    sieveSundaram n = [2*i + 1 | i <- [1..boundary], notIn i numbers]
        where
            boundary = 2*n + 2
            numbers = [ z | y <- [1..boundary], x <- [1..boundary], let z = x + y + 2*x*y, x <= y, z <= boundary ]
            notIn x ys = null $ filter (==x) ys     
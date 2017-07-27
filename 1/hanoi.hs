module Main where

    main :: IO()
    main = putStrLn $ show (hanoi 2 "a" "b" "c")

    type Peg = String
    type Move = (Peg, Peg)

    hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
    --hanoi 2 "a" "b" "c" = [("a","c"), ("a","b"), ("c","b")]
    hanoi 0 _ _ _ = []
    hanoi disk a b c = hanoi (disk - 1) a c b ++ [(a, b)] ++ hanoi (disk-1) c b a
    
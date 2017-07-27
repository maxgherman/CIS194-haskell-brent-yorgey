module Main where

    main :: IO()
    main = putStrLn $ show $ validate 4012888888881882

    split :: Integer -> [Integer] -> [Integer]
    split n arr
        | n == 0 = arr
        | otherwise = split d arr++[m]
            where
                d = div n 10
                m = mod n 10

    toDigits :: Integer -> [Integer]
    toDigits x
        | x <= 0 = []
        | otherwise = split x []

    toDigitsRev :: Integer -> [Integer]
    toDigitsRev x = reverse $ toDigits x

    doubleEveryOther :: [Integer] -> [Integer]
    doubleEveryOther xs = doubleSecond 0 xs [] 

    doubleSecond :: Integer -> [Integer] -> [Integer] -> [Integer]
    doubleSecond _ [] result = result
    doubleSecond n (x:xs) result
        | (mod n 2) == 0 = doubleSecond (n + 1) xs (result++[x])
        | otherwise = doubleSecond (n + 1) xs (result++[x*2])

    sumDigits :: Integer -> [Integer] -> Integer
    sumDigits s [] = s
    sumDigits s (x:xs) = sumDigits (s + sum (toDigits x)) xs
   
    validate :: Integer -> Bool
    validate x = mod s 10 == 0
        where s = sumDigits 0 $ doubleEveryOther (toDigitsRev x)
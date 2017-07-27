{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Streams where

    data Stream a = Cons a ( Stream a)

    streamToList :: Stream a -> [a]
    streamToList (Cons a tail) = a : (streamToList tail)

    instance Show a => Show (Stream a) where
        show a = concat . map (\x -> (show x) ++ " ") $ take 20 (streamToList a)

    streamRepeat :: a -> Stream a
    streamRepeat x = Cons x ( streamRepeat x)

    streamMap :: (a -> b) -> Stream a -> Stream b
    streamMap f (Cons x tail) = Cons (f x) (streamMap f tail)

    streamFromSeed :: (a -> a) -> a -> Stream a
    streamFromSeed f a = Cons a (streamFromSeed f (f a))
  
    nats :: Stream Integer
    nats = streamFromSeed (+1) 0

    interleaveStreams:: Stream a -> Stream a -> Stream a
    interleaveStreams (Cons a tail1) b = Cons a (interleaveStreams b tail1)

    ruler :: Stream Integer
    ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

    ruler' = streamMap divr (streamFromSeed (+1) 1)
        where divr x | odd x = 0
                     | otherwise = divr (x `div` 2) + 1


    x' :: Stream Integer
    x' =  Cons 0 (Cons 1 (streamRepeat 0))

    instance Num (Stream Integer) where
        fromInteger:: Integer -> Stream Integer
        fromInteger a = Cons a (streamRepeat 0)
        negate:: Stream Integer -> Stream Integer
        negate (Cons a tail) = Cons (-a) (negate tail)
        (+):: Stream Integer -> Stream Integer -> Stream Integer
        (+) (Cons a tail1) (Cons b tail2) = Cons (a + b) $ (+) tail1 tail2
        (*):: Stream Integer -> Stream Integer -> Stream Integer
        (*) (Cons a tail1) sb@(Cons b tail2) = Cons (a*b) $ (streamMap (*a) tail2) + (tail1 * sb)

    instance Fractional (Stream Integer) where
        (/):: Stream Integer -> Stream Integer -> Stream Integer
        (/) (Cons a tail1) (Cons b tail2) = q
            where q = Cons (div a b) $ streamMap (`div` b) (tail1 - (q*tail2))
    
    fibs3 :: Stream Integer
    fibs3 = x' / (streamRepeat 1 - x' - x'^2)

    data Matrix = Matrix Integer Integer Integer Integer
        deriving Show

    instance Num Matrix where
        (*):: Matrix -> Matrix -> Matrix
        (*) (Matrix a b c d) (Matrix a1 b1 c1 d1) = Matrix (a*a1 + b*c1) (a*b1 + b*d1) (c*a1 + d*c1) (c*b1 + d*d1)


    extractF:: Matrix -> Integer
    extractF (Matrix _ a _ _) = a

    fib4 :: Integer -> Integer
    fib4 0 = 0
    fib4 n = extractF $ (Matrix 1 1 1 0) ^ n

    fiball :: Stream Integer
    fiball = streamMap fib4 (streamFromSeed (+1) 0)

    instance Num (Stream Matrix) where
        (*):: Stream Matrix -> Stream Matrix -> Stream Matrix
        (*) (Cons a tail1) (Cons b tail2) = Cons (a*b) (tail1 * tail2) 
    
    fiball' :: Stream Integer
    fiball' = streamMap extractF $ Cons (Matrix 0 0 0 0) tail
        where
            f = Matrix 1 1 1 0
            idStream = streamRepeat f
            tail = Cons f (idStream * tail)
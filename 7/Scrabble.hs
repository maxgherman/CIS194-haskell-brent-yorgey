{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

    newtype Score = Score Int
        deriving (Eq, Ord, Show, Num)

    getScore :: Score -> Int
    getScore (Score a) = a

    instance Monoid Score where
        mempty  = Score 0
        mappend x y = Score $ (getScore x) + (getScore y)

    score :: Char -> Score
    score x | elem x "aeilnorstuAEILNORSTU" = 1
            | elem x "dgDG" = 2
            | elem x "bcmpBCMP" = 3
            | elem x "fhvwyFHVWY" = 4
            | elem x "kK" = 5
            | elem x "jJxX" = 8
            | elem x "qQzZ" = 10
            | otherwise = 0

    scoreString :: String -> Score
    scoreString x = foldr mappend mempty (map score x)
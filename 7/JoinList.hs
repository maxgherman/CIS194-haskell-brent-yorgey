{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE InstanceSigs #-}

module JoinList where

    import Data.Monoid
    import Sized
    import Scrabble
    import Buffer

    data JoinList m a = Empty
        | Single m a
        | Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)

    (+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
    (+++) x y = Append (mappend (tag x) (tag y)) x y

    tag :: Monoid m => JoinList m a -> m
    tag Empty = mempty
    tag (Single m _) = m
    tag (Append m _ _) = m

    (!!?) :: [a] -> Int -> Maybe a
    [] !!? _ = Nothing
    _ !!? i | i < 0 = Nothing
    (x:xs) !!? 0 = Just x
    (x:xs) !!? i = xs !!? (i-1)

    jlToList :: JoinList m a -> [a]
    jlToList Empty = []
    jlToList (Single _ a) = [a]
    jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

    indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
    indexJ _ Empty = Nothing
    indexJ i _ | i < 0 = Nothing
    indexJ 0 (Single _ a) = Just a
    indexJ _ (Single _ a) = Nothing
    -- indexJ i (Single _ a) | i == 0 = Just a
    --                       | otherwise = Nothing
    indexJ i (Append m left right) | i < 0 = Nothing
                                   | i >= getSize (size m) = Nothing
                                   | i < leftSize = indexJ i left
                                   | otherwise = indexJ (i - leftSize) right
                                   where leftSize = getSize . size . tag $ left

    dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
    dropJ _ Empty = Empty
    dropJ i _ | i < 0 = Empty
    dropJ 0 r@(Single _ _) = r
    dropJ _ r@(Single _ _) = Empty
    dropJ i (Append m left right) | i > getSize (size m) = Empty
                                  | i < leftSize = Append (mappend (tag lft) (tag right)) lft right
                                  | otherwise = dropJ (i - leftSize) right
                                  where leftSize = getSize . size . tag $ left
                                        lft = dropJ i left

    takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
    takeJ _ Empty = Empty
    takeJ i _ | i <= 0 = Empty
    takeJ _ r@(Single _ _) = r
    takeJ i r@(Append m left right) | i > getSize (size m) = r
                                    | i < leftSize = takeJ i left
                                    | otherwise = Append (mappend (tag left) (tag rht)) left rht
                                    where leftSize = getSize . size . tag $ left
                                          rht = takeJ (i - leftSize) right

    scoreLine :: String -> JoinList Score String
    scoreLine x = Single (scoreString x) x
   

    type BufferList = JoinList (Score, Size) String 
 
    instance Buffer BufferList where
        toString :: BufferList -> String
        toString = unlines . jlToList

        fromString :: String -> BufferList
        fromString = foldr (+++) Empty . items
            where
                items = map (\x -> Single (scoreString x, Size 1) x) . lines

        line :: Int -> BufferList -> Maybe String
        line = indexJ

        replaceLine :: Int -> String -> BufferList -> BufferList
        replaceLine i x b = takeJ i b +++ fromString x +++ dropJ (i + 1) b

        numLines :: BufferList -> Int
        numLines Empty = 0
        numLines (Single _ _)  = 1
        numLines (Append m l r) = (numLines l) + (numLines r)

        value :: BufferList -> Int
        value = getScore . fst . tag
module Folds where

    xor :: [Bool] -> Bool
    xor xs = odd $ foldr summ 0 xs
        where summ x acc = if x == True then acc + 1 else acc

    map' :: (a -> b) -> [a] -> [b]
    map' f = foldr (\x acc -> (f x):acc ) []

    foldl2 :: (a -> b -> a) -> a -> [b] -> a
    foldl2 f a bs = foldr (\b g x -> g (f x b)) id bs a
 
    -- myfoldl :: (a -> b -> a) -> a -> [b] -> a
    -- myfoldl f z [] = z
    -- myfoldl f z (x:xs) =  myfoldl f (f z x) xs
    
    -- myfoldl :: (a -> b -> a) -> a -> [b] -> a
    -- myfoldl f z xs = go z xs
    --     where go acc [] = acc
    --           go acc (x:xs) = go (f acc x) xs 
    
    -- myfoldl :: (a -> b -> a) -> a -> [b] -> a
    -- myfoldl f z xs = go xs z
    --     where go [] acc = acc
    --           go (x:xs) acc = go xs (f acc x) 
    
    --  myfoldl :: (a -> b -> a) -> a -> [b] -> a
    --  myfoldl f z xs = go xs z
    --     where go [] = (\acc -> acc)
    --           go (x:xs) = \acc -> (go xs) (f acc x) 
    
    foldl' f a list = (foldr construct (\acc -> acc) list) a
        where
            construct x r = \acc -> r (f acc x)

    
    

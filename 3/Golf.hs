module Golf where

    import Data.List
    
    skips1 :: [a] -> [[a]]
    skips1 [] = []
    skips1 xs = xs : skips1 (drop 1 xs)

    skips :: [a] -> [[a]]
    skips xs = [ [ x | (x, i) <- zip xs [1..], i `mod` n == 0 ] | n <- [1..length xs]]
    
    ------------------------------------------------------
    
    localMaxima :: [Integer] -> [Integer]
    localMaxima [] = []
    localMaxima [_] = []
    localMaxima xs = map sn . filter isMax $ zips xs
        where
            sn (_, a, _) = a
            isMax (a, b, c) = b > a && b > c
            zips (x1:x2:xs) = zip3 (x1:x2:xs) (x2:xs) xs

    ------------------------------------------------------
   
    histogram :: [Integer] -> String
    histogram xs =
        let rows = reverse [ foldl' (takeNth n) "" rect | n <- [1..max]]
            takeNth n acc xs = acc++(drop (n-1) (take n xs))
            rect = [ x ++ (replicate (max - (length x)) ' ') | x <- hist ]  
            max = maximum $ map length hist
            hist = [ replicate x '*' | x <- [ length $ filter (==n) xs | n <- [0..9] ] ] 
            in unlines $ rows++[replicate 10 '=']++["0123456789"]
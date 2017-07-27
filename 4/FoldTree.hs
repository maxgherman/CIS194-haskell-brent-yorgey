module FoldTree where

    data Tree a = Leaf
                | Node Integer (Tree a) a (Tree a)
        deriving (Show, Eq)

    
    foldTree :: [a] -> Tree a
    foldTree xs = foldr insert Leaf xs 

    height :: Tree a -> Integer
    height Leaf = -1
    height (Node h _ _ _)  = h

    isBalanced :: Tree a -> Bool
    isBalanced Leaf = True
    isBalanced (Node _ left d right) = isOK && isBalanced left && isBalanced right 
        where isOK = abs (height left - height right) <= 1

    insert :: a -> Tree a -> Tree a
    insert a Leaf = Node 0 Leaf a Leaf
    insert a (Node h left d right)
        | height left < height right = Node h (insert a left) d right
        | height left > height right = Node h left d (insert a right)
        | otherwise = Node (heightLeft+1) leftTree d right
            where heightLeft = height leftTree
                  leftTree = insert a left




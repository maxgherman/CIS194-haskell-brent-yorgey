module Party where
    import Employee
    import Data.Monoid
    import Data.Tree
    import Data.List

    main :: IO()
    main = do 
        lst <- fmap read $ readFile "company.txt"
        putStrLn $ format $ maxFun lst
    
    format :: GuestList -> String
    format (GL xs fl) = unlines $ total:(Data.List.sort $ map empName xs)
        where total = "Total fun: " ++ (show fl)
    
    glCons :: Employee -> GuestList -> GuestList
    glCons e@(Emp { empFun = x }) (GL emps fun) = GL (e:emps) (x + fun)

    instance Monoid GuestList where
        mempty  = GL [] 0
        mappend (GL emps1 fun1) (GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)

    moreFun :: GuestList -> GuestList -> GuestList
    moreFun gl1 gl2 | gl1 >= gl2 = gl1
                  | otherwise = gl2

    treeFold :: b -> (b -> a -> b) -> Tree a -> b
    treeFold e f (Node { rootLabel = a, subForest = [] }) = f e a
    treeFold e f (Node { rootLabel = a, subForest = xs}) = foldr (\curr acc -> (treeFold acc f curr) ) (f e a) xs

    node :: Tree Int
    node = ( Node {
                    rootLabel = 1,
                    subForest = [ Node { rootLabel = 2, subForest = [  ]},
                                  Node { rootLabel = 3, subForest = [ Node { rootLabel = 4, subForest = []} ]} ]
                })
   
    nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
    nextLevel e xs = (boss, noBoss)
        where
            boss = glCons e $ mconcat $ map snd xs
            noBoss = mconcat $ foldr (\curr acc -> (funSpread curr):acc) [] xs
            funSpread x = moreFun (fst x) (snd x)

    maxFun :: Tree Employee -> GuestList
    maxFun tree = moreFun (fst pair) (snd pair)
        where 
            pair = pairs tree
            pairs (Node e xs) = nextLevel e $ map pairs xs
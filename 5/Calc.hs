{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

    import ExprT
    import Parser
    import StackVM
    import qualified Data.Map as M

    eval :: ExprT -> Integer
    eval (ExprT.Lit a) = a
    eval (ExprT.Add a b) = (eval a) + (eval b)
    eval (ExprT.Mul a b) = (eval a) * (eval b)

    -- evalM :: Maybe ExprT -> Maybe Integer
    -- evalM (Just a) = Just (eval a)
    -- evalM _ = Nothing

    evalStr :: String -> Maybe Integer
    evalStr s = fmap eval $ parseExp ExprT.Lit ExprT.Add ExprT.Mul s

    class Expr a where
        lit :: Integer -> a
        add :: a -> a -> a
        mul :: a -> a -> a

    instance Expr ExprT where
        lit = ExprT.Lit
        add = ExprT.Add
        mul = ExprT.Mul

    reify :: ExprT -> ExprT
    reify = id

    instance Expr Integer where
        lit = id
        add = (+)
        mul = (*)
        
    instance Expr Bool where
        lit a = if a <= 0 then False else True
        add = (||)
        mul = (&&)

    newtype MinMax = MinMax Integer deriving (Show)

    instance Expr MinMax where
        lit = MinMax
        add (MinMax a) (MinMax b) = MinMax $ max a b
        mul (MinMax a) (MinMax b) = MinMax $ min a b

    newtype Mod7 = Mod7 Integer deriving (Eq, Show)

    instance Expr Mod7 where
        lit a = Mod7 $ mod a 7
        add (Mod7 a) (Mod7 b) = Mod7 $ mod (a + b) 7
        mul (Mod7 a) (Mod7 b) = Mod7 $ mod (a * b) 7

    testExp :: Expr a => Maybe a
    testExp = parseExp lit add mul "(3 * -4) + 5"

    testExp2 :: Expr a => Maybe a
    testExp2 = parseExp lit add mul "3"

    testInteger = testExp :: Maybe Integer
    testBool = testExp :: Maybe Bool
    testMM = testExp :: Maybe MinMax
    testSat = testExp :: Maybe Mod7

    instance Expr StackVM.Program where
        lit a = [StackVM.PushI a]
        add a b = a ++ b ++ [StackVM.Add]
        mul a b = a ++ b ++ [StackVM.Mul]

    compile :: String -> Maybe Program
    compile x = parseExp lit add mul x

    exe :: Maybe Program -> Either String StackVal
    exe (Just a) = stackVM a
    exe Nothing = Right Void

    class HasVars a where
        var :: String -> a

    data VarExprT = VLit Integer
           | VAdd VarExprT VarExprT
           | VMul VarExprT VarExprT
           | Var String
        deriving (Show, Eq)

    instance Expr VarExprT where
        lit = VLit
        add = VAdd
        mul = VMul

    instance HasVars VarExprT where
        var = Var

    instance HasVars (M.Map String Integer -> Maybe Integer) where
        var a = M.lookup a

    instance Expr (M.Map String Integer -> Maybe Integer) where
        lit a = \_ -> Just a
        add a b = \m -> case (a m) of
                            Just r1 -> case (b m) of
                                Just r2 -> Just (r1 + r2)
                                Nothing -> Nothing
                            Nothing -> Nothing
        
        mul a b = \m -> case (a m) of
                            Just r1 -> case (b m) of
                                Just r2 -> Just (r1 * r2)
                                Nothing -> Nothing
                            Nothing -> Nothing

    withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
    withVars vs exp = exp $ M.fromList vs

    --  withVars [("x", 6)] $ add (lit 3) (var "x")
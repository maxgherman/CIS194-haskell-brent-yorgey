{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

first :: (a -> b) -> (a,c) -> (b,c)
first f (a, c) = (f a, c)

instance Functor Parser where
  -- fmap :: (a -> b) -> f a -> f b
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser runParser) = Parser (\x -> map (runParser x))
    where
          map Nothing = Nothing
          map (Just pair) = Just (first f pair)

instance Applicative Parser where
  -- pure :: a -> f a
  pure a = Parser (\x -> Just (a, x))
  -- (<*>) :: f (a -> b) -> f a -> f b
  -- Parser { runParser :: String -> Maybe (a, String) }
  (Parser p1) <*> (Parser p2) = Parser p3
    where
          p3 xs = app1 (p1 xs)
          app1 Nothing = Nothing
          app1 (Just (f, s)) = app2 f (p2 s) 
          app2 _ Nothing = Nothing
          app2 map (Just (b, s)) = Just (map b, s)


abParser :: Parser (Char, Char)
abParser = (,) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (\_-> ()) <$> abParser

intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> (char ' ') <*> posInt

instance Alternative Parser where
  -- empty :: f a
  empty = Parser (\_ -> Nothing)
  -- (<|>) :: f a -> f a -> f a
  (Parser p1) <|> (Parser p2) = Parser p3
    where
        p3 xs = app1 xs (p1 xs)
        app1 _ r@(Just _) = r
        app1 xs Nothing = p2 xs


intOrUppercase :: Parser ()
intOrUppercase = (emp <$> posInt) <|> (emp <$> uppercase)
  where
      emp = \_ -> ()
      uppercase = satisfy isUpper


(.+) = liftA2 (+)    -- addition lifted to some Applicative context
(.*) = liftA2 (*)    -- same for multiplication

-- nondeterministic arithmetic
m = ([4,5] .* pure 2)
n = ([4,5] .* pure 2) .+ [6,1]

m1 = (*) <$> ( (+) <$> Just 3 <*> Just 5) <*> Just 8
m2 = map (\y -> y 3) [(*), (+)] <*> [1, 2]

{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

-- fmap :: (a -> b) -> f a -> f b
-- empty :: f a
-- (<|>) :: f a -> f a -> f a
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b

  

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = liftConcat p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore = liftConcat

liftConcat p = liftA2 (:) p $ zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore $ satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser Atom
parseAtom = spaces *> (parseInteger <|> parseIdent) <* spaces
  where
      parseInteger = N <$> posInt
      parseIdent = I <$> ident
  

parseSExpr :: Parser SExpr
parseSExpr = atom <|> s
  where  
      atom = A <$> parseAtom
      s = char '(' *> spaces *> comb <* spaces <* char ')'
      comb = Comb <$> (oneOrMore parseSExpr)
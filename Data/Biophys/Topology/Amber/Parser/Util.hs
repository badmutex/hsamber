{-# LANGUAGE
  FlexibleInstances
  , TypeSynonymInstances
  #-}

module Data.Biophys.Topology.Amber.Parser.Util where

import Data.Monoid
import Text.ParserCombinators.Parsec


-- | Platform (Windows, *nix) agnostic line terminator
eol :: Parser String
eol = try (string "\n\r")
      <|> try (string "\r\n")
      <|> string "\n"
      <|> string "\r"
      <?> "end of line"


instance Monoid (Parser String) where
    mempty = option "" (many anyChar)
    mappend p1 p2 = do
      p1' <- p1
      p2' <- p2
      return $ p1' ++ p2'
 
-- | Parser concatenation
(<++>) :: (Monoid a) => a -> a -> a
l <++> r = mconcat [l,r]
 

number :: Parser String
number = many digit

positiveInt, negativeInt :: (Integral i, Read i) => Parser i
positiveInt = read `fmap` number
negativeInt = read `fmap` (option "" (string "-") <++> number)
 
-- | Parse (-/+) integers
integral :: (Integral i, Read i) => Parser i
integral = negativeInt <|> positiveInt
 
 
-- | Parse reals
decimal :: (Read f, Fractional f) => Parser f
decimal = do
  ds <- many digit
  char '.'
  ds' <- many digit
  return . read $ ds ++ "." ++ ds'
  <?> "a fractional number"


-- | a comment starts with a semicolon and continues until the end of the line. For example
-- > parse comment [] "; foo bar baz\n"
--  returns 'Right ()'
comment :: Parser String
comment = do char ';'
             anyChar `manyTill` eol

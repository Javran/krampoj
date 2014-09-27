module Language.Scheme.Krampoj.Parser
where

import Control.Applicative hiding (many)
import Text.Parsec hiding ((<|>),letter,digit)
import Text.Parsec.String (Parser)
import Data.Char

digit :: Parser Char
digit = oneOf . concatMap show $ [0..9 :: Int]

specialInitial :: Parser Char
specialInitial = oneOf "!$%&*/:<=>?^_~"

-- | case insensitive char
ciChar :: Char -> Parser Char
ciChar ch = char ch1 <|> char ch2
    where
        ch1 = toLower ch
        ch2 = toUpper ch

letter :: Parser Char
letter = choice . map ciChar $ ['a' .. 'z']

initial :: Parser Char
initial =  letter
       <|> specialInitial
       <?> "initial"

specialSubsequent :: Parser Char
specialSubsequent = oneOf "+-.@"

subsequent :: Parser Char
subsequent =  initial
          <|> digit
          <|> specialSubsequent
          <?> "subsequent"

peculiarIdentifier :: Parser String
peculiarIdentifier =  string "+"
                  <|> string "-"
                  <|> string "..."

identifier :: Parser String
identifier =  ((:) <$> initial <*> many subsequent)
          <|> peculiarIdentifier

module Language.Scheme.Krampoj.Parser
where

import Control.Applicative hiding (many)
import Text.Parsec hiding ((<|>),letter,digit)
import Text.Parsec.String (Parser)
import Data.Char
import Data.Ix
import Data.Function

charBetween :: Char -> Char -> Parser Char
charBetween = curry (satisfy . inRange)

digit :: Parser Char
digit = charBetween '0' '9'

specialInitial :: Parser Char
specialInitial = oneOf "!$%&*/:<=>?^_~"

-- | case insensitive char
ciChar :: Char -> Parser Char
ciChar ch = satisfy (((==) `on` toLower) ch)

letter :: Parser Char
letter = charBetween 'a' 'z' <|> charBetween 'A' 'Z'

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
          <?> "identifier"

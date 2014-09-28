module Language.Scheme.Krampoj.Parser
where

import Control.Applicative hiding (many)
import Text.Parsec hiding ((<|>),letter,digit)
import Text.Parsec.String (Parser)
import Data.Char
import Data.Ix
import Data.Function

data R = R2
       | R8
       | R10
       | R16

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

digitR :: R -> Parser Char
digitR R2  = charBetween '0' '1'
digitR R8  = charBetween '0' '7'
digitR R10 = digit
digitR R16 =  digitR R10
          <|> choice (map ciChar ['a' .. 'f'])

-- TODO: case insensitive string
radixR :: R -> Parser String
radixR R2 = string "#b"
radixR R8 = string "#o"
radixR R10 = string "#d" <|> empty
radixR R16 = string "#x"

exactness :: Parser String
exactness = (char '#' >> ( (char 'i' >> return "#i")
                       <|> (char 'e' >> return "#e")))
         <|> empty

sign :: Parser Char
sign = char '+' <|> char '-' <|> empty

exponentMarker :: Parser Char
exponentMarker = choice (map ciChar "esfdl")

suffix :: Parser String
suffix = suffix' <|> empty
    where
        -- suffix' = <exponent marker> <sign> <digit10>+
        suffix' = (:) <$> exponentMarker
                      <*> ( (:) <$> sign <*> many1 (digitR R10) )

prefix :: R -> Parser String
prefix r = try ( (++) <$> radixR r <*> exactness)
           <|> ( (++) <$> exactness <*> radixR r)

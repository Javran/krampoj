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

ciString :: String -> Parser String
ciString = mapM ciChar

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
radixR R2  = ciString "#b"
radixR R8  = ciString "#o"
radixR R10 = ciString "#d" <|> empty
radixR R16 = ciString "#x"

-- TODO: to live easier with numbers
-- we need to make sure somewhere that
-- everything is in lowercase
exactness :: Parser String
exactness = (char '#' >> ( (ciChar 'i' >> return "#i")
                       <|> (ciChar 'e' >> return "#e")))
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

uinteger :: R -> Parser String
uinteger r = (++) <$> many1 (digitR r) <*> many (char '#')

decimal :: R -> Parser String
decimal R10 = (++) <$> (p1 <|> try p2 <|> p3) <*> suffix
    where
        p1, p2, p3 :: Parser String
        -- p1 parses ". <digit 10>+ #* "
        p1 = do
            _ <- char '.'
            xs <- many1 (digitR R10)
            hs <- many (char '#')
            return $ '.' : xs ++ hs
        -- p2 parses things that begins with "<digit 10>"
        -- backtrack is necessary since the proceeding parser needs it
        p2 = do
            -- <digit 10>+
            xs <- many1 (digitR R10)
            -- '. <digit 10>*' or '#+ .'
            let p21 = (++) <$> string "." <*> many (digitR R10)
                p22 = (++) <$> many1 (char '#') <*> string "."
            ys <- p21 <|> p22
            -- '#*'
            zs <- many (char '#')
            return (xs ++ ys ++ zs)
        -- p3 parses <uinteger 10>
        p3 = uinteger R10
decimal _ = error "not defined by R5RS"

ureal :: R -> Parser String
ureal r =  try (decimal r)
       <|> do xs <- uinteger r
              ys <- option "" ((++) <$> string "/" <*> uinteger r)
              return(xs ++ ys)

real :: R -> Parser String
real r = (:) <$> sign <*> ureal r

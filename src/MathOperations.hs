module MathOperations where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
import Data.Functor.Identity (Identity)

-- Zbrajanje dvaju brojeva
add :: Double -> Double -> Double
add x y = x + y

-- Oduzimanje dvaju brojeva
subtract' :: Double -> Double -> Double
subtract' x y = x - y

-- Množenje dvaju brojeva
multiply :: Double -> Double -> Double
multiply x y = x * y

-- Dijeljenje dvaju brojeva
divide :: Double -> Double -> Either String Double
divide _ 0 = Left "Nije moguće dijeliti s nulom"
divide x y = Right (x / y)

-- Kvadriranje
square :: Double -> Double
square x = x ** 2

-- Potenciranje
power :: Double -> Double -> Double
power x y = x ** y

-- Potenciranje s negativnim eksponentom
negPower :: Double -> Double -> Double
negPower x y = x ** (-y)

-- Logaritmiranje s bazom
logarithm :: Double -> Double -> Either String Double
logarithm x base
  | x <= 0 = Left "Logaritam je definiran samo za pozitivne brojeve"
  | base <= 1 = Left "Baza logaritma mora biti veća od 1"
  | otherwise = Right (logBase base x)

-- Prirodni logaritam
ln :: Double -> Either String Double
ln x
  | x <= 0 = Left "Prirodni logaritam je definiran samo za pozitivne brojeve"
  | otherwise = Right (log x)


-- Postotak nekog broja
percentageOf :: Double -> Double -> Double
percentageOf percent number = (percent / 100) * number

-- Sinus
sin' :: Double -> Double
sin' = sin

-- Kosinus
cos' :: Double -> Double
cos' = cos

-- Tangens
tan' :: Double -> Double
tan' = tan

-- Kvadratni korijen
sqrt' :: Double -> Either String Double
sqrt' x
  | x < 0 = Left "Korijen je definiran samo za pozitivne brojeve"
  | otherwise = Right (sqrt x)

-- Pi
pi' :: Double
pi' = pi

-- Apsolutna vrijednost
absolute :: Double -> Double
absolute = abs

-- Eulerov broj
e' :: Double
e' = exp 1





-- Parsiranje izraza

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens = Token.parens lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

expressionParser :: Parser Double
expressionParser = buildExpressionParser operators term

term :: Parser Double
term = parens expressionParser
    <|> try float
    <|> (fromInteger <$> integer)

operators :: [[Operator String () Identity Double]]
operators = [ [Prefix (reservedOp "-" >> return negate) ]
            , [Infix (reservedOp "^" >> return power) AssocRight]
            , [Infix (reservedOp "*" >> return (*)) AssocLeft,
               Infix (reservedOp "/" >> return (/)) AssocLeft]
            , [Infix (reservedOp "+" >> return (+)) AssocLeft,
               Infix (reservedOp "-" >> return (-)) AssocLeft]
            ]

parseExpression :: String -> Either ParseError Double
parseExpression input = parse expressionParser "" input

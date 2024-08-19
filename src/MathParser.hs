module MathParser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
import Data.Functor.Identity (Identity)

-- DEFINICIJA FUNKCIJA

-- Kvadratni korijen s rukovanjem greškom
sqrt' :: Double -> Either String Double
sqrt' x
  | x < 0 = Left "Korijen je definiran samo za pozitivne brojeve"
  | otherwise = Right (sqrt x)

-- Logaritam s proizvoljnom bazom
logarithmWithBase :: Double -> Double -> Either String Double
logarithmWithBase base x
  | x <= 0 = Left "Logaritam je definiran samo za pozitivne brojeve"
  | base <= 1 = Left "Baza logaritma mora biti veća od 1"
  | otherwise = Right (logBase base x)

-- Logaritam baza 10
logBase10 :: Double -> Double
logBase10 = logBase 10

-- Prirodni logaritam s rukovanjem greškom
ln :: Double -> Either String Double
ln x
  | x <= 0 = Left "Prirodni logaritam je definiran samo za pozitivne brojeve"
  | otherwise = Right (log x)

-- Postotak nekog broja
percentageOf :: Double -> Double -> Double
percentageOf percent number = (percent / 100) * number

-- Funkcija za rukovanje greškama
handleError :: Either String Double -> Double
handleError = either (const 0) id

-- DEFINICIJA PARSERA

-- Lexer definicija
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

reserved :: String -> Parser ()
reserved = Token.reserved lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

-- Definiranje parsera za funkcije
expressionParser :: Parser Double
expressionParser = buildExpressionParser operators term

term :: Parser Double
term = try float
    <|> (fromInteger <$> integer)
    <|> parens expressionParser
    <|> try (do
            reserved "log"
            base <- integer -- Ovdje prepoznaje bazu umjesto crtice
            x <- parens expressionParser -- Prepoznaje izraz unutar zagrada
            return $ handleError $ logarithmWithBase (fromInteger base) x)
    <|> (reserved "log" >> logBase10 <$> parens expressionParser)
    <|> (reserved "sqrt" >> (handleError . sqrt' <$> parens expressionParser))
    <|> (reserved "ln" >> (handleError . ln <$> parens expressionParser))
    <|> (reserved "sin" >> sin <$> parens expressionParser)
    <|> (reserved "cos" >> cos <$> parens expressionParser)
    <|> (reserved "tan" >> tan <$> parens expressionParser)
    <|> (reserved "abs" >> abs <$> parens expressionParser)
    <|> (reserved "pi" >> return pi)
    <|> (reserved "e" >> return (exp 1))

operators :: [[Operator String () Identity Double]]
operators = [ [Prefix (reservedOp "-" >> return negate) ]
            , [Infix (reservedOp "**" >> return (**)) AssocRight]
            , [Infix (reservedOp "*" >> return (*)) AssocLeft,
               Infix (reservedOp "/" >> return (/)) AssocLeft]
            , [Infix (reservedOp "+" >> return (+)) AssocLeft,
               Infix (reservedOp "-" >> return (-)) AssocLeft]
            ]

parseExpression :: String -> Either ParseError Double
parseExpression input = parse (whiteSpace >> expressionParser) "" input

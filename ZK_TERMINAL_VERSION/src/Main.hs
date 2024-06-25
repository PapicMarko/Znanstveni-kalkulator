{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Parsec hiding (many, space) -- Uklonite hiding listu koja skriva <|>
import Text.Parsec (try, (<|>), many, parse, Parsec, runParser, space)
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Data.Functor.Identity (Identity)
import System.IO (hFlush, stdout)
import Control.Monad (unless)
import Control.Exception (catch, SomeException)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import MathOperations (add, subtract', multiply, divide, power, square, logarithm, sin', cos', tan', sqrt', pi', absolute, exp', ln, negativePower)

-- Define the lexer
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef

parens :: Parser a -> Parser a
parens = Token.parens lexer

integer :: Parser Integer
integer = Token.integer lexer

float :: Parser Double
float = Token.float lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

-- Define the expression parser
exprParser :: Double -> Parser Double
exprParser ans = buildExpressionParser (table ans) (factor ans)

table :: Double -> [[Operator String () Identity Double]]
table ans = [ [Prefix (Token.reservedOp lexer "-" >> return negate)]
            , [Infix  (Token.reservedOp lexer "**" >> return power) AssocRight]
            , [Infix  (Token.reservedOp lexer "*" >> return multiply) AssocLeft,
               Infix  (Token.reservedOp lexer "/" >> return (\x y -> either (const 0) id (divide x y))) AssocLeft]
            , [Infix  (Token.reservedOp lexer "+" >> return add) AssocLeft,
               Infix  (Token.reservedOp lexer "-" >> return subtract') AssocLeft]
            ]

factor :: Double -> Parser Double
factor ans = try float
         <|> (fromInteger <$> integer)
         <|> parens (exprParser ans)
         <|> (Token.reservedOp lexer "sqrt" >> (either (const 0) id . sqrt' <$> parens (exprParser ans)))
         <|> (Token.reservedOp lexer "log" >> (parens (exprParser ans) >>= \x -> return (logBase 10 x)))
         <|> (Token.reservedOp lexer "log" >> do { base <- factor ans; whiteSpace; x <- factor ans; return (either (const 0) id (logarithm base x)) })
         <|> (Token.reservedOp lexer "sin" >> sin' <$> parens (exprParser ans))
         <|> (Token.reservedOp lexer "cos" >> cos' <$> parens (exprParser ans))
         <|> (Token.reservedOp lexer "tan" >> tan' <$> parens (exprParser ans))
         <|> (Token.reservedOp lexer "abs" >> absolute <$> parens (exprParser ans))
         <|> (Token.reservedOp lexer "square" >> square <$> parens (exprParser ans))
         <|> (Token.reserved lexer "pi" >> return pi')
         <|> (Token.reserved lexer "e" >> return (exp 1))
         <|> (Token.reserved lexer "ln" >> (either (const 0) id . ln <$> parens (exprParser ans)))
         <|> (Token.reserved lexer "neg" >> do { base <- parens (exprParser ans); whiteSpace; x <- factor ans; return (negativePower base x) })
         <|> (Token.reserved lexer "ans" >> return ans)

evaluateExpression :: Double -> String -> Either ParseError Double
evaluateExpression ans = parse (whiteSpace >> exprParser ans) ""

-- Function to clear the terminal screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

main :: IO ()
main = do
    putStrLn "Dobrodosli u znanstveni kalkulator! Unesite izraz za izracunavanje:"
    ansRef <- newIORef 0.0
    repl ansRef `catch` handler ansRef

repl :: IORef Double -> IO ()
repl ansRef = do
    putStr "Unesite izraz: "
    hFlush stdout
    input <- getLine
    unless (input == "exit") $ do
        if input == "AC" || input == "ac"
            then do
                writeIORef ansRef 0.0
                putStrLn "Svi unosi su obrisani."
            else if input == "clear"
                then do
                    clearScreen
                else do
                    ans <- readIORef ansRef
                    let result = evaluateExpression ans input
                    case result of
                        Left err -> putStrLn $ "Greska: " ++ show err
                        Right val -> do
                            putStrLn $ "Rezultat: " ++ show val
                            writeIORef ansRef val
        repl ansRef `catch` handler ansRef

handler :: IORef Double -> SomeException -> IO ()
handler ansRef ex = putStrLn ("Došlo je do greske: " ++ show ex) >> repl ansRef

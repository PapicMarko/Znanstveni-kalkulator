{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Text.Parsec hiding ((<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Data.Functor.Identity (Identity)
import MathOperations (add, subtract', multiply, divide, power, square, logarithm, sin', cos', tan', sqrt', pi', absolute)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))

void :: Functor f => f a -> f ()
void = fmap (const ())

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
exprParser :: Parser Double
exprParser = buildExpressionParser table factor

table :: [[Operator String () Identity Double]]
table = [ [Prefix (Token.reservedOp lexer "-" >> return negate)]
        , [Infix  (Token.reservedOp lexer "*" >> return multiply) AssocLeft,
           Infix  (Token.reservedOp lexer "/" >> return (\x y -> either (const 0) id (divide x y))) AssocLeft]
        , [Infix  (Token.reservedOp lexer "+" >> return add) AssocLeft,
           Infix  (Token.reservedOp lexer "-" >> return subtract') AssocLeft]
        , [Infix  (Token.reservedOp lexer "**" >> return power) AssocLeft]
        ]

factor :: Parser Double
factor = try float
     <|> (fromInteger <$> integer)
     <|> parens exprParser
     <|> (Token.reservedOp lexer "sqrt" >> (either (const 0) id . sqrt' <$> parens exprParser))
     <|> (Token.reservedOp lexer "log" >> (parens exprParser >>= \x -> return (logBase 10 x)))
     <|> (Token.reservedOp lexer "log" >> do { base <- factor; whiteSpace; x <- factor; return (either (const 0) id (logarithm base x)) })
     <|> (Token.reservedOp lexer "sin" >> sin' <$> parens exprParser)
     <|> (Token.reservedOp lexer "cos" >> cos' <$> parens exprParser)
     <|> (Token.reservedOp lexer "tan" >> tan' <$> parens exprParser)
     <|> (Token.reservedOp lexer "abs" >> absolute <$> parens exprParser)
     <|> (Token.reservedOp lexer "square" >> square <$> parens exprParser)
     <|> (Token.reserved lexer "pi" >> return pi')

-- Evaluate a mathematical expression from a string
evaluateExpression :: String -> Either ParseError Double
evaluateExpression = parse (whiteSpace >> exprParser) ""

main :: IO ()
main = do
    static <- loadStatic
    startGUI defaultConfig { jsPort = Just 8023, jsStatic = Just static } setup

setup :: Window -> UI ()
setup window = do
    _ <- return window # set UI.title "Znanstveni kalkulator"

    -- Add CSS styles directly in the HTML
    let css = unlines
            [ "body {"
            , "    font-family: Arial, sans-serif;"
            , "    background-color: #f2f2f2;"
            , "    color: #333;"
            , "    display: flex;"
            , "    justify-content: center;"
            , "    align-items: center;"
            , "    height: 100vh;"
            , "    margin: 0;"
            , "}"
            , ".container {"
            , "    background-color: #fff;"
            , "    padding: 20px;"
            , "    border-radius: 10px;"
            , "    box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);"
            , "    max-width: 500px;"
            , "    width: 100%;"
            , "    display: flex;"
            , "    flex-direction: column;"
            , "    align-items: center;"
            , "}"
            , ".input {"
            , "    width: 100%;"
            , "    padding: 10px;"
            , "    margin-bottom: 20px;"
            , "    border: 1px solid #ccc;"
            , "    border-radius: 5px;"
            , "    font-size: 16px;"
            , "    height: 50px;"
            , "    resize: none;"
            , "}"
            , ".button-container {"
            , "    display: grid;"
            , "    grid-template-columns: repeat(4, 1fr);"
            , "    gap: 10px;"
            , "    width: 100%;"
            , "    margin-bottom: 20px;"
            , "}"
            , ".button, .button-op, .button-num {"
            , "    width: 100%;"
            , "    padding: 10px;"
            , "    background-color: #007bff;"
            , "    color: white;"
            , "    border: none;"
            , "    border-radius: 5px;"
            , "    cursor: pointer;"
            , "    font-size: 16px;"
            , "    text-align: center;"
            , "}"
            , ".button:hover, .button-op:hover, .button-num:hover {"
            , "    background-color: #0056b3;"
            , "}"
            , ".button-power {"
            , "    width: 100%;"
            , "    padding: 10px;"
            , "    background-color: #007bff;"
            , "    color: white;"
            , "    border: none;"
            , "    border-radius: 5px;"
            , "    cursor: pointer;"
            , "    font-size: 16px;"
            , "    text-align: center;"
            , "    font-family: Arial, sans-serif;"
            , "}"
            , ".button:hover, .button-op:hover, .button-num:hover, .button-power:hover {"
            , "    background-color: #0056b3;"
            , "}"
            , ".result {"
            , "    font-size: 20px;"
            , "    font-weight: bold;"
            , "    margin-top: 20px;"
            , "    text-align: center;"
            , "}"
            ]

    -- Create style element
    styleElement <- UI.mkElement "style" # set text css

    -- GUI elements
    input <- UI.textarea #. "input" # set (attr "placeholder") "Unesite izraz"
    resultLabel <- UI.span #. "result" # set text "Rezultat će biti prikazan ovdje"
    calculateButton <- UI.button #. "button" # set text "="

    -- Operator buttons
    addButton <- UI.button #. "button-op" # set text "+"
    subButton <- UI.button #. "button-op" # set text "-"
    mulButton <- UI.button #. "button-op" # set text "*"
    divButton <- UI.button #. "button-op" # set text "/"
    logButton <- UI.button #. "button-op" # set text "log"
    sqrtButton <- UI.button #. "button-op" # set text "√"
    sinButton <- UI.button #. "button-op" # set text "sin"
    cosButton <- UI.button #. "button-op" # set text "cos"
    tanButton <- UI.button #. "button-op" # set text "tan"
    powerButton <- UI.button #. "button-power" # set html "<span>x<sup>y</sup></span>"
    squareButton <- UI.button #. "button-op" # set text "x²"
    absButton <- UI.button #. "button-op" # set text "|x|"
    piButton <- UI.button #. "button-op" # set text "π"
    lparenButton <- UI.button #. "button-op" # set text "("
    rparenButton <- UI.button #. "button-op" # set text ")"

    -- Number buttons
    oneButton <- UI.button #. "button-num" # set UI.text "1"
    twoButton <- UI.button #. "button-num" # set UI.text "2"
    threeButton <- UI.button #. "button-num" # set UI.text "3"
    fourButton <- UI.button #. "button-num" # set UI.text "4"
    fiveButton <- UI.button #. "button-num" # set UI.text "5"
    sixButton <- UI.button #. "button-num" # set UI.text "6"
    sevenButton <- UI.button #. "button-num" # set UI.text "7"
    eightButton <- UI.button #. "button-num" # set UI.text "8"
    nineButton <- UI.button #. "button-num" # set UI.text "9"
    zeroButton <- UI.button #. "button-num" # set UI.text "0"
    dotButton <- UI.button #. "button-num" # set UI.text "."

    -- Arrange elements in the window
    void $ getBody window #+
        [ element styleElement
        , UI.div #. "container" #+
            [ element input
            , UI.div #. "button-container" #+
                [ element oneButton, element twoButton, element threeButton, element addButton
                , element fourButton, element fiveButton, element sixButton, element subButton
                , element sevenButton, element eightButton, element nineButton, element mulButton
                , element zeroButton, element dotButton, element lparenButton, element rparenButton
                , element logButton, element sqrtButton, element sinButton, element cosButton
                , element tanButton, element powerButton, element divButton
                , element absButton, element squareButton, element piButton
                ]
            , element calculateButton
            , element resultLabel
            ]
        ]

    -- Set button click events for operators and numbers
    let appendOp op = do
            current <- get value input
            void $ element input # set value (current ++ op)

    let appendNum num = do
            current <- get value input
            void $ element input # set value (current ++ num)

    let appendFunc func = do
            current <- get value input
            void $ element input # set value (current ++ func ++ "()")

    let appendLog = do
            current <- get value input
            void $ element input # set value (current ++ "log() ")

    let appendPi = do
            current <- get value input
            void $ element input # set value (current ++ "pi")

    on UI.click oneButton $ \_ -> appendNum "1"
    on UI.click twoButton $ \_ -> appendNum "2"
    on UI.click threeButton $ \_ -> appendNum "3"
    on UI.click fourButton $ \_ -> appendNum "4"
    on UI.click fiveButton $ \_ -> appendNum "5"
    on UI.click sixButton $ \_ -> appendNum "6"
    on UI.click sevenButton $ \_ -> appendNum "7"
    on UI.click eightButton $ \_ -> appendNum "8"
    on UI.click nineButton $ \_ -> appendNum "9"
    on UI.click zeroButton $ \_ -> appendNum "0"
    on UI.click dotButton $ \_ -> appendNum "."

    on UI.click addButton $ const $ appendOp "+"
    on UI.click subButton $ const $ appendOp "-"
    on UI.click mulButton $ const $ appendOp "*"
    on UI.click divButton $ const $ appendOp "/"
    on UI.click logButton $ const appendLog
    on UI.click sqrtButton $ const $ appendFunc "sqrt"
    on UI.click sinButton $ const $ appendFunc "sin"
    on UI.click cosButton $ const $ appendFunc "cos"
    on UI.click tanButton $ const $ appendFunc "tan"
    on UI.click powerButton $ const $ appendOp "**"
    
    -- Update for square and absolute buttons
    on UI.click squareButton $ \_ -> do
        current <- get value input
        void $ element input # set value (current ++ "**2")

    on UI.click absButton $ \_ -> do
        current <- get value input
        void $ element input # set value ("abs(" ++ current ++ ")")

    on UI.click piButton $ const appendPi
    on UI.click lparenButton $ const $ appendOp "("
    on UI.click rparenButton $ const $ appendOp ")"

    -- Set button click event for calculate
    on UI.click calculateButton $ \_ -> do
        inputExpr <- get value input
        let result = evaluateExpression inputExpr
        case result of
            Left err -> void $ element resultLabel # set text ("Greška: " ++ show err)
            Right val -> void $ element resultLabel # set text ("Rezultat: " ++ show val)

loadStatic :: IO FilePath
loadStatic = do
    -- Create a temporary directory for static files
    dir <- getTemporaryDirectory
    let staticDir = dir </> "static"
    createDirectoryIfMissing True staticDir
    -- Write CSS content to a file in the static directory
    let cssContent = unlines
            [ "body {"
            , "    font-family: Arial, sans-serif;"
            , "    background-color: #f2f2f2;"
            , "    color: #333;"
            , "    display: flex;"
            , "    justify-content: center;"
            , "    align-items: center;"
            , "    height: 100vh;"
            , "    margin: 0;"
            , "}"
            , ".container {"
            , "    background-color: #fff;"
            , "    padding: 20px;"
            , "    border-radius: 10px;"
            , "    box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);"
            , "    max-width: 500px;"
            , "    width: 100%;"
            , "    display: flex;"
            , "    flex-direction: column;"
            , "    align-items: center;"
            , "}"
            , ".input {"
            , "    width: 100%;"
            , "    padding: 10px;"
            , "    margin-bottom: 20px;"
            , "    border: 1px solid #ccc;"
            , "    border-radius: 5px;"
            , "    font-size: 16px;"
            , "    height: 50px;"
            , "    resize: none;"
            , "}"
            , ".button-container {"
            , "    display: grid;"
            , "    grid-template-columns: repeat(4, 1fr);"
            , "    gap: 10px;"
            , "    width: 100%;"
            , "    margin-bottom: 20px;"
            , "}"
            , ".button, .button-op, .button-num {"
            , "    width: 100%;"
            , "    padding: 10px;"
            , "    background-color: #007bff;"
            , "    color: white;"
            , "    border: none;"
            , "    border-radius: 5px;"
            , "    cursor: pointer;"
            , "    font-size: 16px;"
            , "    text-align: center;"
            , "}"
            , ".button:hover, .button-op:hover, .button-num:hover {"
            , "    background-color: #0056b3;"
            , "}"
            , ".button-power {"
            , "    width: 100%;"
            , "    padding: 10px;"
            , "    background-color: #007bff;"
            , "    color: white;"
            , "    border: none;"
            , "    border-radius: 5px;"
            , "    cursor: pointer;"
            , "    font-size: 16px;"
            , "    text-align: center;"
            , "    font-family: Arial, sans-serif;"
            , "}"
            , ".button:hover, .button-op:hover, .button-num:hover, .button-power:hover {"
            , "    background-color: #0056b3;"
            , "}"
            , ".result {"
            , "    font-size: 20px;"
            , "    font-weight: bold;"
            , "    margin-top: 20px;"
            , "    text-align: center;"
            , "}"
            ]
    writeFile (staticDir </> "style.css") cssContent
    return staticDir

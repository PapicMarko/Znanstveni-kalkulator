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
import MathOperations (add, subtract', multiply, divide, power, square, logarithm, sin', cos', tan', sqrt', pi', absolute, e', ln, percentageOf, negPower)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))
import Data.IORef (newIORef, readIORef, writeIORef)

void :: Functor f => f a -> f ()
void = fmap (const ())

-- Definiranje leksera
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

-- Definiranje operatora
exprParser :: Double -> Parser Double
exprParser ans = buildExpressionParser (table ans) (factor ans)

table :: p -> [[Operator String () Identity Double]]
table _ = [ [Prefix (Token.reservedOp lexer "-" >> return negate)]
          , [Infix  (Token.reservedOp lexer "%" >> return percentageOf) AssocLeft]
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
         <|> (Token.reservedOp lexer "ln" >> (either (const 0) id . ln <$> parens (exprParser ans)))
         <|> (Token.reservedOp lexer "sin" >> sin' <$> parens (exprParser ans))
         <|> (Token.reservedOp lexer "cos" >> cos' <$> parens (exprParser ans))
         <|> (Token.reservedOp lexer "tan" >> tan' <$> parens (exprParser ans))
         <|> (Token.reservedOp lexer "abs" >> absolute <$> parens (exprParser ans))
         <|> (Token.reservedOp lexer "square" >> square <$> parens (exprParser ans))
         <|> (Token.reserved lexer "pi" >> return pi')
         <|> (Token.reserved lexer "e" >> return e')
         <|> (Token.reserved lexer "ans" >> return ans)
         <|> (Token.reserved lexer "neg" >> do { base <- factor ans; whiteSpace; x <- factor ans; return (negPower base x) })

parseExpression :: Double -> String -> Either ParseError Double
parseExpression ans input = parse (whiteSpace >> exprParser ans) "" input

-- Funkcija za pokretanje GUI aplikacije
main :: IO ()
main = do
    static <- loadStatic
    startGUI defaultConfig { jsPort = Just 8023, jsStatic = Just static } setup

setup :: Window -> UI ()
setup window = void $ do
    _ <- return window # set UI.title "Znanstveni kalkulator"
    lastResult <- UI.liftIO $ newIORef 0.0

    -- Dodavanje CSS-a direktno u HTML
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
            , "    max-width: 800px;"
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
            , "    display: flex;"
            , "    width: 100%;"
            , "    justify-content: space-between;"
            , "}"
            , ".functions-container {"
            , "    display: grid;"
            , "    grid-template-columns: repeat(3, 1fr);"
            , "    gap: 10px;"
            , "}"
            , ".numbers-container {"
            , "    display: grid;"
            , "    grid-template-columns: repeat(4, 1fr);"
            , "    gap: 10px;"
            , "}"
            , ".button, .button-op, .button-num {"
            , "    width: 100%;"
            , "    padding: 10px;"
            , "    background-color: #CCCCCC;"
            , "    color: black;"
            , "    border: none;"
            , "    border-radius: 5px;"
            , "    cursor: pointer;"
            , "    font-size: 16px;"
            , "    text-align: center;"
            , "}"
            , ".button:hover, .button-op:hover, .button-num:hover {"
            , "    background-color: #202123;"
            , "}"
            , ".button-power {"
            , "    width: 100%;"
            , "    padding: 10px;"
            , "    background-color: #CCCCCC;"
            , "    color: black;"
            , "    border: none;"
            , "    border-radius: 5px;"
            , "    cursor: pointer;"
            , "    font-size: 16px;"
            , "    text-align: center;"
            , "    font-family: Arial, sans-serif;"
            , "}"
            , ".button:hover, .button-op:hover, .button-num:hover, .button-power:hover {"
            , "    background-color: #73747A;"
            , "}"
            , ".calculate-button {"
            , "    width: 40%;"
            , "    padding: 10px;"
            , "    background-color: #1433E1;"
            , "    color: white;"
            , "    border: none;"
            , "    border-radius: 5px;"
            , "    cursor: pointer;"
            , "    font-size: 16px;"
            , "    text-align: center;"
            , "}"
            , ".result {"
            , "    font-size: 20px;"
            , "    font-weight: bold;"
            , "    margin-top: 20px;"
            , "    text-align: center;"
            , "}"
            , ".calculate-button:hover {"
            , "    background-color: #202123;"
            , "}"
            , ".control-buttons-container {"
            , "    display: flex;"
            , "    justify-content: flex-end;"
            , "    margin-top: 10px;"
            , "    margin-bottom: 10px;"
            , "    width: 100%;"
            , "    gap: 10px;"
            , "}"
            , ".control-buttons-container button {"
            , "    width: 10%;"
            , "    padding: 10px;"
            , "    background-color: #1433E1;"
            , "    color: white;"
            , "    border: none;"
            , "    border-radius: 20px;"
            , "    cursor: pointer;"
            , "    font-size: 16px;"
            , "    text-align: center;"
            , "}"
            , ".control-buttons-container button:hover {"
            , "    background-color: #202123;"
            , "}"
            ]

    -- Izrada style elemenata
    styleElement <- UI.mkElement "style" # set text css

    -- GUI elementi
    input <- UI.textarea #. "input" # set (attr "placeholder") "Unesite izraz"
    resultLabel <- UI.span #. "result" # set text "Rezultat će biti prikazan ovdje"
    calculateButton <- UI.button #. "calculate-button" # set text "="

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
    ansButton <- UI.button #. "button-op" # set text "ans"
    lparenButton <- UI.button #. "button-op" # set text "("
    rparenButton <- UI.button #. "button-op" # set text ")"
    acButton <- UI.button #. "button-op" # set text "AC"
    backspaceButton <- UI.button #. "button-op" # set text "⌫"
    percentButton <- UI.button #. "button-op" # set text "%"
    lnButton <- UI.button #. "button-op" # set text "ln"
    eButton <- UI.button #. "button-op" # set text "e"
    negPowerButton <- UI.button #. "button-op" # set text "x^-y"

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

    -- Control buttons container
    controlButtonsContainer <- UI.div #. "control-buttons-container" #+
        [ element acButton, element backspaceButton ]

    -- Posloži elemente u GUI
    void $ getBody window #+
        [ element styleElement
        , UI.div #. "container" #+
            [ element input
            , UI.div #. "button-container" #+
                [ UI.div #. "functions-container" #+
                    [ element squareButton, element powerButton, element negPowerButton
                    , element absButton,element piButton, element sqrtButton
                    , element sinButton, element cosButton, element tanButton
                    , element logButton, element lnButton, element eButton
                    ]
                , UI.div #. "numbers-container" #+
                    [ element nineButton, element eightButton, element sevenButton, element mulButton
                    , element fourButton, element fiveButton, element sixButton, element divButton
                    , element oneButton, element twoButton, element threeButton, element addButton
                    , element zeroButton, element dotButton, element ansButton, element subButton
                    , element lparenButton, element rparenButton, element percentButton
                    ]
                ]
            , element controlButtonsContainer
            , element calculateButton
            , element resultLabel
            ]
        ]

    -- Postavi funkcije za gumbe
    let appendOp op = do
            current <- get value input
            void $ element input # set value (current ++ op)

    let appendNum num = do
            current <- get value input
            void $ element input # set value (current ++ num)

    let appendFunc func = do
            current <- get value input
            let newVal = current ++ func ++ "("
            void $ element input # set value newVal

    let appendLog = do
            current <- get value input
            let newVal = current ++ "log("
            void $ element input # set value newVal

    let appendLn = do
            current <- get value input
            let newVal = current ++ "ln("
            void $ element input # set value newVal

    let appendE = do
            current <- get value input
            void $ element input # set value (current ++ "e")

    let appendPi = do
            current <- get value input
            void $ element input # set value (current ++ "pi")

    let appendAns = do
            current <- get value input
            lastAns <- UI.liftIO $ readIORef lastResult
            void $ element input # set value (current ++ show lastAns)

    let backspace = do
            current <- get value input
            if not (null current)
                then do
                    let newVal = init current
                    void $ element input # set value newVal
                else return ()

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
    on UI.click percentButton $ const $ appendOp "%"
    on UI.click logButton $ const appendLog
    on UI.click lnButton $ const appendLn
    on UI.click sqrtButton $ const $ appendFunc "sqrt"
    on UI.click sinButton $ const $ appendFunc "sin"
    on UI.click cosButton $ const $ appendFunc "cos"
    on UI.click tanButton $ const $ appendFunc "tan"
    on UI.click powerButton $ const $ appendOp "**"
    on UI.click eButton $ const appendE
    on UI.click backspaceButton $ const backspace
    on UI.click negPowerButton $ const $ appendFunc "neg"

    -- Update za kvadrat i absolutnu vrijednost
    on UI.click squareButton $ \_ -> do
        current <- get value input
        void $ element input # set value (current ++ "**2")

    on UI.click absButton $ \_ -> do
        current <- get value input
        void $ element input # set value ("abs(" ++ current)

    on UI.click piButton $ const appendPi
    on UI.click ansButton $ const appendAns
    on UI.click lparenButton $ const $ appendOp "("
    on UI.click rparenButton $ const $ appendOp ")"
    on UI.click acButton $ \_ -> do
        void $ element input # set value ""
        void $ element resultLabel # set text "Rezultat će biti prikazan ovdje"

    -- Postavljanje funkcije za izračun
    on UI.click calculateButton $ \_ -> do
        inputExpr <- get value input
        lastAns <- UI.liftIO $ readIORef lastResult
        let result = evaluateExpression lastAns inputExpr
        case result of
            Left err -> void $ element resultLabel # set text ("Greška: " ++ show err)
            Right val -> do
                void $ element resultLabel # set text ("Rezultat: " ++ show val)
                UI.liftIO $ writeIORef lastResult val

evaluateExpression :: Double -> String -> Either ParseError Double
evaluateExpression lastAns input = parseExpression lastAns input

loadStatic :: IO FilePath
loadStatic = do
    -- Izrada privremenog direktorija za statičke datoteke
    dir <- getTemporaryDirectory
    let staticDir = dir </> "static"
    createDirectoryIfMissing True staticDir
    return staticDir

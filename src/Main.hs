{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import MathParser (parseExpression)  -- Uvozimo samo potrebne funkcije
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing)
import System.FilePath ((</>))


void :: Functor f => f a -> f ()
void = fmap (const ())

-- Funkcija za pokretanje GUI aplikacije
main :: IO ()
main = do
    static <- loadStatic
    startGUI defaultConfig { jsPort = Just 8023, jsStatic = Just static } setup

-- Funkcija za učitavanje CSS datoteke
loadCSS :: FilePath -> IO String
loadCSS path = readFile path

setup :: Window -> UI ()
setup window = void $ do
    _ <- return window # set UI.title "Znanstveni kalkulator"
    lastResult <- liftIO $ newIORef 0.0

    -- Učitavanje sadržaja style.css datoteke
    cssContent <- liftIO $ loadCSS "static/css/style.css"

    -- Umetanje CSS-a direktno u HTML
    let cssElement = UI.mkElement "style" # set UI.text cssContent
    _ <- getHead window #+ [cssElement]

    -- Definiranje UI elemenata
    input <- UI.textarea #. "input" # set (attr "placeholder") "Unesite izraz"
    resultLabel <- UI.span #. "result" # set UI.text "Rezultat ce biti prikazan ovdje"
    calculateButton <- UI.button #. "calculate-button" # set UI.text "="

    -- Operator buttons
    addButton <- UI.button #. "button-op" # set UI.text "+"
    subButton <- UI.button #. "button-op" # set UI.text "-"
    mulButton <- UI.button #. "button-op" # set UI.text "*"
    divButton <- UI.button #. "button-op" # set UI.text "/"
    logButton <- UI.button #. "button-op" # set UI.text "log"
    sqrtButton <- UI.button #. "button-op" # set UI.text "√"
    sinButton <- UI.button #. "button-op" # set UI.text "sin"
    cosButton <- UI.button #. "button-op" # set UI.text "cos"
    tanButton <- UI.button #. "button-op" # set UI.text "tan"
    powerButton <- UI.button #. "button-power" # set UI.html "<span>x<sup>y</sup></span>"
    squareButton <- UI.button #. "button-op" # set UI.text "x²"
    absButton <- UI.button #. "button-op" # set UI.text "|x|"
    piButton <- UI.button #. "button-op" # set UI.text "π"
    ansButton <- UI.button #. "button-op" # set UI.text "ans"
    lparenButton <- UI.button #. "button-op" # set UI.text "("
    rparenButton <- UI.button #. "button-op" # set UI.text ")"
    acButton <- UI.button #. "button-op" # set UI.text "AC"
    backspaceButton <- UI.button #. "button-op" # set UI.text "⌫"
    percentButton <- UI.button #. "button-op" # set UI.text "%"
    lnButton <- UI.button #. "button-op" # set UI.text "ln"
    eButton <- UI.button #. "button-op" # set UI.text "e"
    negPowerButton <- UI.button #. "button-op" # set UI.text "x^-y"

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
    _ <- getBody window #+
        [ UI.div #. "container" #+
            [ element input
            , UI.div #. "button-container" #+
                [ UI.div #. "functions-container" #+
                    [ element squareButton, element powerButton, element negPowerButton
                    , element absButton, element piButton, element sqrtButton
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

    -- Funkcije za gumbe
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
    on UI.click logButton $ \_ -> appendOp "log _()"
    on UI.click lnButton $ \_ -> appendOp "ln()"
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
        void $ element resultLabel # set text "Rezultat ce biti prikazan ovdje"

    -- Postavljanje funkcije za izračun
    on UI.click calculateButton $ \_ -> do
        inputExpr <- get value input
        let result = parseExpression inputExpr
        case result of
            Left err -> void $ element resultLabel # set text ("Greska: " ++ show err)
            Right val -> do
                void $ element resultLabel # set text ("Rezultat: " ++ show val)
                UI.liftIO $ writeIORef lastResult val

-- Funkcija za statički sadržaj
loadStatic :: IO FilePath
loadStatic = do
    temp <- getTemporaryDirectory
    let dir = temp </> "static"
    createDirectoryIfMissing True dir
    return dir

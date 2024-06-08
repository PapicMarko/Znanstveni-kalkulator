{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import MathOperations (add, subtract', multiply, divide, power, logarithm, sin', cos', tan', sqrt')
import Text.Read (readMaybe)

void :: Functor f => f a -> f ()
void = fmap (const ())

main :: IO ()
main = do
    startGUI defaultConfig { jsPort = Just 8023 } setup

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
            , "    max-width: 400px;"
            , "    width: 100%;"
            , "}"
            , ".input {"
            , "    width: calc(50% - 10px);"
            , "    padding: 10px;"
            , "    margin-bottom: 10px;"
            , "    border: 1px solid #ccc;"
            , "    border-radius: 5px;"
            , "    font-size: 16px;"
            , "}"
            , ".button-container {"
            , "    display: flex;"
            , "    flex-wrap: wrap;"
            , "    justify-content: space-between;"
            , "    margin-bottom: 10px;"
            , "}"
            , ".button {"
            , "    flex: 1 1 calc(33.333% - 10px);"
            , "    margin: 5px;"
            , "    padding: 10px;"
            , "    background-color: #007bff;"
            , "    color: white;"
            , "    border: none;"
            , "    border-radius: 5px;"
            , "    cursor: pointer;"
            , "    font-size: 16px;"
            , "    text-align: center;"
            , "}"
            , ".button:hover {"
            , "    background-color: #0056b3;"
            , "}"
            , ".result {"
            , "    font-size: 18px;"
            , "    font-weight: bold;"
            , "    margin-top: 10px;"
            , "    text-align: center;"
            , "}"
            ]

    -- GUI elements
    input1 <- UI.input #. "input" # set (attr "placeholder") "Unesite prvi broj"
    input2 <- UI.input #. "input" # set (attr "placeholder") "Unesite drugi broj"
    resultLabel <- UI.span #. "result" # set text "Rezultat će biti prikazan ovdje"

    addButton <- UI.button #. "button" # set text "Zbroji"
    subButton <- UI.button #. "button" # set text "Oduzmi"
    mulButton <- UI.button #. "button" # set text "Pomnoži"
    divButton <- UI.button #. "button" # set text "Podijeli"
    powButton <- UI.button #. "button" # set text "Potencija"
    logButton <- UI.button #. "button" # set text "Logaritam"
    sinButton <- UI.button #. "button" # set text "Sin"
    cosButton <- UI.button #. "button" # set text "Cos"
    tanButton <- UI.button #. "button" # set text "Tan"
    sqrtButton <- UI.button #. "button" # set text "Korijen"

    -- Create style element
    styleElement <- UI.mkElement "style" # set text css

    -- Arrange elements in the window
    void $ getBody window #+
        [ element styleElement
        , UI.div #. "container" #+
            [ element input1
            , element input2
            , UI.div #. "button-container" #+ [element addButton, element subButton, element mulButton, element divButton, element powButton, element logButton, element sinButton, element cosButton, element tanButton, element sqrtButton]
            , element resultLabel
            ]
        ]

    -- Hide the second input initially
    _ <- element input2 # set UI.style [("display", "none")]

    -- Set button click events
    on UI.click addButton $ \_ -> performDoubleInputOperation input1 input2 resultLabel add
    on UI.click subButton $ \_ -> performDoubleInputOperation input1 input2 resultLabel subtract'
    on UI.click mulButton $ \_ -> performDoubleInputOperation input1 input2 resultLabel multiply
    on UI.click divButton $ \_ -> performDoubleInputOperationSafe input1 input2 resultLabel divide
    on UI.click powButton $ \_ -> performDoubleInputOperation input1 input2 resultLabel power
    on UI.click logButton $ \_ -> performDoubleInputOperationSafe input1 input2 resultLabel logarithm
    on UI.click sinButton $ \_ -> performSingleInputOperation input1 resultLabel sin'
    on UI.click cosButton $ \_ -> performSingleInputOperation input1 resultLabel cos'
    on UI.click tanButton $ \_ -> performSingleInputOperation input1 resultLabel tan'
    on UI.click sqrtButton $ \_ -> performSingleInputOperationSafe input1 resultLabel sqrt'

    -- Show or hide the second input based on the button clicked
    let doubleInputButtons = [addButton, subButton, mulButton, divButton, powButton, logButton]
        singleInputButtons = [sinButton, cosButton, tanButton, sqrtButton]
    
    mapM_ (\btn -> on UI.click btn $ \_ -> element input2 # set UI.style [("display", "inline-block")]) doubleInputButtons
    mapM_ (\btn -> on UI.click btn $ \_ -> element input2 # set UI.style [("display", "none")]) singleInputButtons

-- Function for double-argument operations that do not return an error
performDoubleInputOperation :: Element -> Element -> Element -> (Double -> Double -> Double) -> UI ()
performDoubleInputOperation input1 input2 resultLabel op = do
    mval1 <- get value input1
    mval2 <- get value input2
    let maybeX = readMaybe mval1 :: Maybe Double
        maybeY = readMaybe mval2 :: Maybe Double
    case (maybeX, maybeY) of
        (Just x, Just y) -> void $ element resultLabel # set text ("Rezultat: " ++ show (op x y))
        _ -> return () -- Do nothing if the input is invalid

-- Function for double-argument operations that can return an error
performDoubleInputOperationSafe :: Element -> Element -> Element -> (Double -> Double -> Either String Double) -> UI ()
performDoubleInputOperationSafe input1 input2 resultLabel op = do
    mval1 <- get value input1
    mval2 <- get value input2
    let maybeX = readMaybe mval1 :: Maybe Double
        maybeY = readMaybe mval2 :: Maybe Double
    case (maybeX, maybeY) of
        (Just x, Just y) -> 
            case op x y of
                Right result -> void $ element resultLabel # set text ("Rezultat: " ++ show result)
                Left err -> void $ element resultLabel # set text ("Greška: " ++ err)
        _ -> return () -- Do nothing if the input is invalid


-- Function for single-argument operations that do not return an error
performSingleInputOperation :: Element -> Element -> (Double -> Double) -> UI ()
performSingleInputOperation input resultLabel op = do
    mval <- get value input
    let maybeX = readMaybe mval :: Maybe Double
    case maybeX of
        Just x -> void $ element resultLabel # set text ("Rezultat: " ++ show (op x))
        _ -> return () -- Do nothing if the input is invalid

-- Function for single-argument operations that can return an error
performSingleInputOperationSafe :: Element -> Element -> (Double -> Either String Double) -> UI ()
performSingleInputOperationSafe input resultLabel op = do
    mval <- get value input
    let maybeX = readMaybe mval :: Maybe Double
    case maybeX of
        Just x -> 
            case op x of
                Right result -> void $ element resultLabel # set text ("Rezultat: " ++ show result)
                Left err -> void $ element resultLabel # set text ("Greška: " ++ err)
        _ -> return () -- Do nothing if the input is invalid



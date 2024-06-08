module MathOperations where

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
divide _ 0 = Left "Cannot divide by zero"
divide x y = Right (x / y)

-- Potenciranje
power :: Double -> Double -> Double
power x y = x ** y

-- Logaritmiranje s bazom
logarithm :: Double -> Double -> Either String Double
logarithm x base
  | x <= 0 = Left "Logarithm undefined for non-positive numbers"
  | base <= 1 = Left "Logarithm base must be greater than 1"
  | otherwise = Right (logBase base x)


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
  | x < 0 = Left "Square root undefined for negative numbers"
  | otherwise = Right (sqrt x)

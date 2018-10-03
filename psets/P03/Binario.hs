module Binario where

 -- Ejercicio 1.1
data Binario = BaseUno | Uno Binario | Cero Binario deriving (Eq)
instance Show Binario where
    show BaseUno = "1"
    show (Cero b) = show b ++ "0"
    show (Uno b) = show b ++ "1"

-- Ejercicio 1.2
natToBin :: Int -> Binario
natToBin 1 = BaseUno
natToBin n
    | n `mod` 2 == 0 = Cero $ natToBin division
    | otherwise = Uno $ natToBin division
    where division = n `div` 2

-- Ejercicio 1.3
binToNat :: Binario -> Int
binToNat b = sum [2^e * d | (e, d) <- expPairs]
    where digits = binDigits b
          maxExp = length digits - 1
          expPairs = zip [0..maxExp] digits

-- Ejercicio 1.4
sucesor :: Binario -> Binario
sucesor BaseUno = Cero BaseUno
sucesor (Uno b) = Cero (sucesor b)
sucesor (Cero b) = Uno b

-- ========== Auxiliary functions ==========

-- binDigits: Return a list with the digits contained in a Binary
--            (least significant digit first)
binDigits :: Binario -> [Int]
binDigits BaseUno = [1]
binDigits (Cero b) = 0 : binDigits b
binDigits (Uno b) = 1 : binDigits b


-- --Ejercicio 1.3
--  binToNat :: Binario -> Int
--  --Tu código va aquí
--  binToNat = error "Borra esta linea"

-- --Ejercicio 1.4
--  sucesor :: Binario -> Binario
--  --Tu código va aquí
--  sucesor = error "Borra esta linea"

-- --Ejercicio 1.5
--  bitsEncendidos :: Binario -> Int
--  --Tu código va aquí
--  bitsEncendidos = error "Borra esta linea"

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
binToNat BaseUno = 1
binToNat (Cero b) = (2 * (binToNat b))
binToNat (Uno b) = 1 + (2 * (binToNat b))

-- Ejercicio 1.4
sucesor :: Binario -> Binario
sucesor BaseUno = Cero BaseUno
sucesor (Uno b) = Cero (sucesor b)
sucesor (Cero b) = Uno b

-- Ejercicio 1.5
bitsEncendidos :: Binario -> Int
bitsEncendidos BaseUno = 1
bitsEncendidos (Cero b) = bitsEncendidos b
bitsEncendidos (Uno b) = 1 + bitsEncendidos b

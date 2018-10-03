module Binario where

 --Ejercicio 1.1
 data Binario = BaseUno | Uno Binario | Cero Binario deriving (Eq, Show)


 instance Show Binario where
  show BaseUno = "1"
  show (Cero b) = show b ++ "0"
  show (Uno b) = show b ++ "1"

--Ejercicio 1.2
 natToBin :: Int -> Binario
 --Tu código va aquí
 natToBin = error "Borra esta linea"

--Ejercicio 1.3
 binToNat :: Binario -> Int
 --Tu código va aquí
 binToNat = error "Borra esta linea"

--Ejercicio 1.4
 sucesor :: Binario -> Binario
 --Tu código va aquí
 sucesor = error "Borra esta linea"

--Ejercicio 1.5
 bitsEncendidos :: Binario -> Int
 --Tu código va aquí
 bitsEncendidos = error "Borra esta linea"

module Practica3 where

import Binario

-- Ejercicio 2.1
binarios :: [Int] -> [Binario]
binarios l = map natToBin l

-- Ejercicio 2.2
pares :: [Binario] -> [Binario]
pares l = filter (\b -> case b of (Cero _) -> True; _ -> False) l

-- Ejercicio 2.3
tooLong :: [String] -> [String]
tooLong l = filter (\x -> length x <= 7) l

-- Ejercicio 2.4
sFibonacci :: Int -> [Int]
sFibonacci n = map (fibonacci) [0..n]

-- Ejercicio 2.5
quitaElemento :: (Eq a) => [a] -> a -> [a]
quitaElemento l e = filter (/=e) l

-- | Auxiliary functions

-- Fibonacci
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci $ n - 1) + (fibonacci $ n - 2)

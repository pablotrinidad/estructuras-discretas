-- Estructuras Discretas 2019 - 1
-- Práctica 02
-- Pablo Trinidad: 419004279

module Practica2 where

-- 1.- Definición de listas

--1.1.- Naturales (Siguiendo el estándar ISO 80000-2)
nat = [0..]
--1.2.- Multiplos de diez.
multiplosDiez = [0, 10..]
--1.3.- Potencias de 2
potenciasDos = [2 ^ x | x <- [0..]]
--1.4.- Números pares.
pares = [0, 2..]
--1.5.- años desde el año de tu nacimiento.
anosVividos = [1997..2018]


--2. Funciones

-- 2.1.- Fibonacci:
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci $ n - 1) + (fibonacci $ n - 2)

-- 2.2.- Elemento:
elemento :: (Eq a) => [a] -> a -> Bool
elemento [] _ = False
elemento (x:xs) e = if x == e then True else elemento xs e

-- 2.3.- Suma de los elementos de una lista:
sumaLista ::(Num a) => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

-- 2.4.- Meses:
meses :: [Int] -> [String]
meses [] = []
meses (1:xs) = "Enero" : meses xs
meses (2:xs) = "Febrero" : meses xs
meses (3:xs) = "Marzo" : meses xs
meses (4:xs) = "Abril" : meses xs
meses (5:xs) = "Mayo" : meses xs
meses (6:xs) = "Junio" : meses xs
meses (7:xs) = "Julio" : meses xs
meses (8:xs) = "Agosto" : meses xs
meses (9:xs) = "Septiembre" : meses xs
meses (10:xs) = "Octubre" : meses xs
meses (11:xs) = "Noviembre" : meses xs
meses (12:xs) = "Diciembre" : meses xs
meses (_:xs) = "No existe ese mes!" : meses xs

-- 2.5.- Divisores propios:
divisoresPropios :: Int -> [Int]
divisoresPropios n = [x | x <- [1..(n - 1)], n `mod` x == 0]

-- 2.6.- Números perfectos:
esPerfecto :: Int -> Bool
esPerfecto n = (sumaLista $ divisoresPropios n) == n

-- 2.7.- Números amigos:
sonAmigos :: Int -> Int -> Bool
sonAmigos a b = (sumaLista $ divisoresPropios a) == b && (sumaLista $ divisoresPropios b) == a

-- 2.8.- Supersuma:
supersuma :: Int -> Int
supersuma 0 = 0
supersuma x = x `mod` 10 + (supersuma $ x `div` 10)

-- 2.9 - Japonés:
japones :: Int -> String
japones 0 = "rei"
japones 1 = "ichi"
japones 2 = "ni"
japones 3 = "san"
japones 4 = "yon"
japones 5 = "go"
japones 6 = "roku"
japones 7 = "nana"
japones 8 = "haci"
japones 9 = "kyu"
japones 10 = "ju"
japones n
    | (n `mod` 10) > 0 = japones (n `div` 10) ++ " ju " ++ japones (n `mod` 10)
    | otherwise = japones (n `div` 10) ++ " ju"

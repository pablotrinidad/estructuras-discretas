module Practica3 where

import Binario

-- Ejercicio 2.1
binarios :: [Int] -> [Binario]
binarios l = map natToBin l

--Ejercicio 2.2
pares :: [Binario] -> [Binario]
pares l = filter (\b -> case b of (Cero _) -> True; _ -> False) l

--Ejercicio 2.3
-- tooLong :: [String] -> [String]
-- tooLong l = filter (length > )

-- --Ejercicio 2.4
--  sFibonacci :: Int -> [Int]
--  --Tu código va aquí
--  sFibonacci = error "Borrar esta linea"

-- --Ejercicio 2.5
--  quitaElemento :: (Eq a) => [a] -> a -> [a]
--  --Tu código va aquí
--  quitaElemeto = error "Borrar esta linea"

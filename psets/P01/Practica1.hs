-- ESTRUCTURAS DISCRETAS 2019-1
--Práctica 1

--Ejercicio 1.1:
areaCirculo :: Float -> Float
areaCirculo r = pi * (r ** 2)

--Ejercicio 1.2:
distancia :: (Float , Float ) -> (Float , Float ) -> Float
--Tu código va aquí
distancia (x1, y1) (x2, y2) = sqrt (((x1 - x2)**2) + ((y1 - y2)**2))

--Ejercicio 1.3:
imp :: Bool -> Bool -> Bool
imp p q= (not p) || q

--Ejercicio 1.4:
xor :: Bool -> Bool -> Bool
xor p q = not (p == q)

--Ejercicio 1.5:
mes :: Int -> String
mes 1 = "Enero"
mes 2 = "Febrero"
mes 3 = "Marzo"
mes 4 = "Abril"
mes 5 = "Mayo"
mes 6 = "Junio"
mes 7 = "Julio"
mes 8 = "Agosto"
mes 9 = "Septiembre"
mes 10 = "Octubre"
mes 11 = "Noviembre"
mes 12 = "Diciembre"

--Ejercicio 1.6:
calculadora :: String -> (Int ,Int) -> Int
calculadora "first" (a, b) = a
calculadora "last" (a, b) = b
calculadora "sum" (a, b) = a + b
calculadora "rest" (a, b) = a - b
calculadora "mul" (a, b) = a * b
calculadora "div" (a, b) = div a b
calculadora "pow" (a, b) = a ^ b

--Ejercicio 1.7:
loki :: Int -> Bool -> String
loki temp True = if (30 > temp) && (20 < temp) then "Sale jugar" else "No sale a jugar"
loki temp False = if (25 > temp) && (15 < temp) then "Sale jugar" else "No sale a jugar"

--Ejercicio 1.8:
monos :: Bool -> Bool -> String
monos a b = if a == b then "Hay problemas" else "No hay problemas"

suma :: Int -> Int -> Int
suma a 0 = a
suma 0 b = b
suma a b = suma (succ a) (pred b)

--Ejercicio 1.9:
multiplica :: Int -> Int -> Int
multiplica 0 _ = 0
multiplica _ 0 = 0
multiplica a b = suma (multiplica a (pred b)) a

--Ejercicio 1.10:
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia 0 _ = 1
potencia a b = multiplica (potencia a (pred b)) a

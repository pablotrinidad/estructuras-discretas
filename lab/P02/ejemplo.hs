--Funció que suma recursivamente 2 números Naturales
--ESTA ES LA FUNCCIÓN NECESARIA PARA LA PRÁCTICA 1
suma :: Int -> Int -> Int 
suma 0 n = n
suma n m = suma (pred n) (succ m)

--Función que calcula el factorial de un número 
factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * (factorial (pred n))

--Función que verifica si un número es par
par :: Int -> Bool
par 0 = True 
par n = impar (pred n) --Doble recursión 

--Función que verifica si un número es impar
impar :: Int -> Bool 
impar 0 = False
impar n = par (pred n) --Doble recursión

--Función que regresa la cabeza (Head) de una lista
car :: [a] -> a
car [] = error "No hay elementos"
car (x:xs) = x

--Función que regresa la cola (Tail) de una lista
cdr :: [a] -> [a]
cdr [] = error "La lista no tiene cola"
cdr (x:xs) = xs

--Función que no regresa el último elemento de una lista
ultimo :: [a] -> a
ultimo [] = error "No hay elementos"
ultimo [e] = e
ultimo (x:xs) = ultimo xs

--Función que toma los primeros n elementos de una lista
toma :: [a] -> Int -> [a]
toma [] _ = []
toma l 0 = []
toma (x:xs) n = x:(toma xs (pred n))

--Función que quita los primeros n elementos de una lista
deja :: [a] -> Int -> [a]
deja [] _ = []
deja l 0 = l
deja (x:xs) n = deja xs (pred n)

--Función que mezcla 2 listas
merge :: [a] -> [a] -> [a]
merge [] l = l
merge l [] = l
merge (x:xs) (y:ys) = x:(y:(merge xs ys))

--Función que concatena 2 listas
concatena :: [a] ->[a] -> [a]
concatena [] ys = ys
concatena (x:xs) ys = x : (concatena xs ys)

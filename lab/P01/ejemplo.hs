--Función que dice su un número es par o no.
par :: Int -> Bool
par n = (mod n 2) == 0  

--Función que dice si un número es impar o no.
impar :: Int -> Bool
impar n = not (par n)

--Función que utiliza el teorema de Pitagoras para calcular la hipotenusa de un triángulo, recibe los catetos de éste.
pitagoras :: Float -> Float -> Float 
pitagoras  x y = sqrt ((x ** 2) + (y ** 2))

--Función que incrementa en 1 el número que recibe.
incrementa :: Int -> Int
incrementa n = succ n 

--Función que incrementa en 1 cada entrada de la tupla que recibe.
incrementaTupla :: (Int,Int) -> (Int,Int)
incrementaTupla (n,m) = (incrementa n, incrementa m)

--Función que nos regresa -1 si el número es negativo, 1 si es positivo y 0 si es 0
signo :: Int -> Int
signo 0 = 0
signo n = if (n > 0) then 1 else -1

{- Función que nos regresa la pareja de la persona que recibe
   las parejas son las siguientes : 
	Juan - Romina
	Luis - Maria
	Mariano - Rosaura

Observación: La relación ser pareja es reflexiva.
-}
pareja :: String -> String
pareja "Juan" = "Romina"
pareja "Romina" = "Juan"
pareja "Luis" = "Maria"
pareja "Maria" = "Luis"
pareja "Mariano" = "Rosaura"
pareja "Rosaura" = "Mariano"
pareja s = "no tiene pareja" --caso de error, por si recibimos un nombre de alguien que no tenga pareja

--Función que te dice si tu número es de la suerte o no (Sólo e 7 es de la suerte)
lucky :: Int -> String  
lucky 7 = "NUMERO DE LA SUERTE!!"  
lucky x = "Mejor suerte para la próxima dude."

--Función que dice cómo se escribe un número de 1 al 5 
sayMe :: Int -> String  
sayMe 1 = "Uno"  
sayMe 2 = "Dos"  
sayMe 3 = "Tres"  
sayMe 4 = "Cuatro"  
sayMe 5 = "Cinco"  
sayMe x = "Este número esta fuera de rango" --caso de error por si el número recibido no esta entre 1 y 5 










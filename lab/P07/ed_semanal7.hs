-- Pablo Trinidad - 419004279

module Prop where

import Data.List

--DEFINICIONES
-- Tipo de dato para representar las expresiones de la lógica proposicional
data Prop = Verdadero
          | Falso
          | Var String
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Impl Prop Prop
          | Syss Prop Prop
          deriving (Eq,Ord)

-- Sinónimo para representar el estado
type Estado = [(String, Prop)]

--Instancia de Show para el tipo Prop, para que sea legible lo que se imprime en consola
-- NO DEFINE COMPORTAMIENTO
instance Show Prop where
 show Verdadero = "V"
 show Falso = "F"
 show (Var x) = x
 show (Neg p) = "¬ " ++ show p
 show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
 show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
 show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
 show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

--EJERCICIOS

--EJERCICIO 1
variables :: Prop -> [String]
variables Verdadero = []
variables Falso = []
variables (Var a) = [a]
variables (Neg a) = nub $ (variables a) ++ []
variables (Conj a b) = nub $ (variables a) ++ (variables b) ++ []
variables (Disy a b) = nub $ (variables a) ++ (variables b) ++ []
variables (Impl a b) = nub $ (variables a) ++ (variables b) ++ []
variables (Syss a b) = nub $ (variables a) ++ (variables b) ++ []

--EJERCICIO 2
quitaRepetidosPar ::(Eq a) => [(a,b)] -> [(a,b)]
quitaRepetidosPar [] = []
quitaRepetidosPar (x:xs)
    | (fst x) `elem` (map (\e -> fst e) remaining) = remaining
    | otherwise = x:remaining
    where remaining = quitaRepetidosPar $ reverse xs

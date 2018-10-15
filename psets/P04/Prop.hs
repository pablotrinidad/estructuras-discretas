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

-- Instancia de Show para el tipo Prop, para que sea legible lo que se imprime en consola
-- NO DEFINE COMPORTAMIENTO
instance Show Prop where
    show Verdadero = "V" -- V
    show Falso = "F" -- F
    show (Var x) = x -- P
    show (Neg p) = "¬ " ++ show p -- ¬ P
    show (Conj p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")" -- (P ∧ Q)
    show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")" -- (P ∨ Q)
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")" -- (P → Q)
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")" -- (P ↔ Q)


-- EQUIVALENCIAS LÓGICAS

-- Define la elminación de la implicación y la equivalencia
eliminacion :: Prop -> Prop
eliminacion (Impl a b) = (Disy (Neg (eliminacion a)) (eliminacion b))
eliminacion (Syss a b) = (Conj (eliminacion (Impl a b)) (eliminacion (Impl b a)))
eliminacion (Conj a b) = (Conj (eliminacion a) (eliminacion b))
eliminacion (Disy a b) = (Disy (eliminacion a) (eliminacion b))
eliminacion (Neg a) = (Neg (eliminacion a))
eliminacion p = p

-- Define las reglas de De Morgan
deMorgan :: Prop -> Prop
deMorgan (Neg (Disy a b)) = (Conj (deMorgan (Neg a)) (deMorgan (Neg b)))
deMorgan (Neg (Conj a b)) = (Disy (deMorgan (Neg a)) (deMorgan (Neg b)))
deMorgan (Impl a b) = (Impl (deMorgan a) (deMorgan b))
deMorgan (Syss a b) = (Syss (deMorgan a) (deMorgan b))
deMorgan (Neg (Neg (Var a))) = (Var a)
deMorgan p = p

-- -- EVALUACIÓN Y ANÁLISIS SINTÁCTICO DE EXPRESIONES

-- Evalúa una proposicón en el estado recibido
interp :: Prop -> Estado -> Bool
interp _ [] = error "El estado no incluye la proposición"
interp Verdadero _ = True
interp Falso _ = False
interp (Var a) (x:xs)
    | a == (fst x) = (\p -> p == Verdadero) (snd x)
    | otherwise = interp (Var a) xs
interp (Neg a) s = not $ interp a s
interp (Conj a b) s = (interp a s) && (interp b s)
interp (Disy a b) s = (interp a s) || (interp b s)
interp p s = interp (eliminacion p) s


-- Dada una lista de estados y una proposición, entonctrar el valor de dicha proposicón
-- buscar :: Prop -> Estado -> Bool
-- buscar _ [] = error "El estado no incluye la proposición"
-- buscar (Var a)(x:xs)
--     | a == (fst x) = (\p -> p == Verdadero) (snd x)
--     | otherwise = buscar (Var a) xs

-- -- Ejercicio 2.2
-- truthTable :: Prop -> String
-- -- Aquí va tu código
-- truthTable = error "Función no definida"

-- -- Ejercicio 2.3
-- correcto :: [Prop] -> Prop -> Bool
-- -- Aquí va tu código
-- correcto = error "Función no definida"


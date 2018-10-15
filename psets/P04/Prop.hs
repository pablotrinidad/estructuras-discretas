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

-- Recibe una proposición y la evalúa en todos los estados para identificar si es tautología, contradicción o contingencia
truthTable :: Prop -> String
truthTable p
    | and r = "Tautología"
    | or r = "Contingencia"
    | otherwise = "Contradicción"
        -- Evalua proposición en cada estado
        where r = [interp p x | x <- states]
                -- Genera los estados
                where states = stateGenerator vars
                        -- Obtiene la lista de variables involucradas
                        where vars = (foldl (\l e -> if e `elem` l then l else l ++ [e]) [] . varExtractor) p

-- Recibe una lista de proposiciones y una conclusión. Si
correcto :: [Prop] -> Prop -> Bool
correcto p c = (truthTable form) == "Tautología"
    where form = (Impl (recursiveConj p) c)

-- Utilidades

-- Función para extraer las variables dentro de una proposicón
varExtractor :: Prop -> [String]
varExtractor Verdadero = []
varExtractor Falso = []
varExtractor (Var a) = [a]
varExtractor (Neg a) = varExtractor a ++ []
varExtractor (Conj a b) = (varExtractor a) ++ (varExtractor b) ++ []
varExtractor (Disy a b) = (varExtractor a) ++ (varExtractor b) ++ []
varExtractor (Impl a b) = (varExtractor a) ++ (varExtractor b) ++ []
varExtractor (Syss a b) = (varExtractor a) ++ (varExtractor b) ++ []

-- Generador de estados. Recibe una lista de variables y las regresa combinadas con cada estado.
stateGenerator :: [String] -> [Estado]
stateGenerator [] = [[]]
stateGenerator (x:xs) = [l ++ [(x, v)] | l <- stateGenerator xs, v <- [Verdadero, Falso]]

-- Dada una lista de fórmulas, regresa la conjunción de ellas
recursiveConj :: [Prop] -> Prop
recursiveConj [] = Verdadero
recursiveConj (x:xs) = (Conj x (recursiveConj xs))

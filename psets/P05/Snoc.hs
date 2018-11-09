module Snoc where

--Tipo de dato algebraico para definir listas Snoc
data SnocList a = Mt
    | Snoc (SnocList a) a
    deriving (Eq, Ord, Show)

-- addSnoc: Recibe una lista snoc, un elemento e y agrega a e como último elemento de la lista
addSnoc :: SnocList a -> a -> SnocList a
addSnoc l e = Snoc l e

-- ultimo: Regresa el último elemento de una lista snoc
ultimo :: SnocList a -> a
ultimo (Snoc a b) = b

-- rest: Regresa el principio de una lista snoc
resto :: SnocList a -> SnocList a
resto (Snoc a b) = a

-- cabeza: Regrsa la cabeza de una lista snoc
cabeza :: SnocList a -> a
cabeza (Snoc Mt a) = a
cabeza (Snoc a b) = cabeza a

-- cola: Regresa todos los elementos de una lista snoc, excepto la cola
cola :: SnocList a -> SnocList a
cola (Snoc Mt a) = Mt
cola (Snoc a b) = Snoc (cola a) b

-- longitud: Regresa la cantidad de elementos de una lista snoc
longitud :: SnocList a -> Int
longitud (Snoc Mt a) = 1
longitud (Snoc a b) = 1 + longitud a

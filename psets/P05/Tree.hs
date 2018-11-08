module Tree where

-- Tipo de dato Algebraico para definir Árboles Binarios
data BinaryTree a = Void
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

-- addTree: Agrega un elemento a un árbol binario ordenado preservando el orden
addTree :: (Ord a) => BinaryTree a -> a -> BinaryTree a
addTree Void e = (Node Void e Void)
addTree (Node l c r) e
    | e < c = Node (addTree l e) c r
    | e > c = Node l c (addTree r e)
    | otherwise = Node l c r

-- inorder: Regresa una lista ordenada (asc) de los elementos del árbol
inorder :: BinaryTree a -> [a]
inorder Void = []
inorder (Node l c r) = inorder l ++ c : inorder r

-- preorder: Regresa una lista de los elementos del árbol obtenidos siguiendo la recursión
preorder :: BinaryTree a -> [a]
preorder Void = []
preorder (Node l c r) = [c] ++ (preorder l) ++ (preorder r)

-- --Ejercicio 2.4
-- postorder :: BinaryTree a -> [a]
-- postorder = error "Falta Implementar"

-- --Ejercicio 2.5
-- maximo :: (Ord a) => BinaryTree a -> a
-- maximo = error "Falta Implementar"

-- --Ejercicio 2.6
-- minimo :: (Ord a) => BinaryTree a -> a
-- minimo = error "Falta Implementar"

-- --Ejercicio 2.7
-- busca :: (Ord a) => a -> BinaryTree a -> Bool
-- busca = error "Falta Implementar"

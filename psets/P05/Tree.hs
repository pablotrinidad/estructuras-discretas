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

-- preorder: Regresa una lista de los elementos del árbol obtenidos siguiendo la recursión (centro, izq, der)
preorder :: BinaryTree a -> [a]
preorder Void = []
preorder (Node l c r) = [c] ++ (preorder l) ++ (preorder r)

-- postorder: Regresa una lista de los elementos del árbol obtenidos siguiendo la recursión (izq, der, centro)
postorder :: BinaryTree a -> [a]
postorder Void = []
postorder (Node l c r) = (postorder l) ++ (postorder r) ++ [c]

-- maximo: Regresa el elemento más grande del árbol
maximo :: (Ord a) => BinaryTree a -> a
maximo (Node _ e Void) = e
maximo (Node _ c r) = maximo r

-- minimo: Regresa el elemento más pequeño del árbol
minimo :: (Ord a) => BinaryTree a -> a
minimo (Node Void e _) = e
minimo (Node l c _) = minimo l

-- busca: Regresa un booleano indicando si el elemento es parte del árbol
busca :: (Ord a) => a -> BinaryTree a -> Bool
busca e (Node Void c Void) = if c == e then True else False
busca e (Node l c r)
    | e < c = busca e l
    | e > c = busca e r
    | otherwise = True

compara :: Int -> Int -> String
compara n m
    | n > m = "GT"
    | n < m = "LT"
    | n == m = "EQ"

iniciales :: String -> String -> String
iniciales nombre apellido = [n] ++ ". " ++ [a]
    where n = head nombre
          a = head apellido

palindromo :: String -> Bool
palindromo s =
    let rs = reverse s
    in s == rs


data Figura = Circulo Float
    | Cuadrado Float
    | Rectangulo Float Float
    | Triangulo Float Float deriving (Show, Eq)


data Lista a = Empty | Cons a (Lista a)

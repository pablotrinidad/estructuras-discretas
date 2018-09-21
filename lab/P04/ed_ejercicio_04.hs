-- Pablo A. Trinidad Paz - 419004279

data Figura = Circulo Float
    | Cuadrado Float
    | Rectangulo Float Float
    | Triangulo Float Float  deriving (Show, Eq)


area :: Figura -> Float
area (Circulo r) = pi * r ^ 2
area (Cuadrado l) = l ^ 2
area (Rectangulo b h) = b * h
area (Triangulo b h) = (b * h) / 2

loki :: Int -> Bool -> String
loki t True
    | (t < 30) && ( t > 20) = "Sale jugar"
    | otherwise = "No sale a Jugar"
loki t False
    | (t < 25) && (t > 15) = "Sale a jugar"
    | otherwise = "No sale a jugar"


suma1 :: [Int] -> [Int]
suma1 a = map (+ 1)a

areaHeron :: Float -> Float -> Float -> Float
areaHeron a b c = sqrt (s * (s - a) * (s - b) * (s - c))
    where s = (a + b + c) / 2

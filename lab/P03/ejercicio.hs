-- Máximo común divisor
mcd :: Int -> Int -> Int
mcd a b = last [x | x <- [1..(max a b)], (mod a x) == 0, (mod b x) == 0]

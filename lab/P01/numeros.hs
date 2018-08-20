-- Even numbers
isEven :: Int -> Bool
isEven n = if mod n 2 == 0 then True else False

-- Pythagoras
pyth :: Float -> Float -> Float
pyth x y = sqrt ((x ** 2) + (y ** 2))

-- Integer increment by one
increase :: Int -> Int
increase a = a + 1

-- Circle
circleArea :: Float -> Float
circleArea r = pi * (r ** 2)

-- Numbers addition
addition :: Float -> Float -> Float
addition x y = x + y

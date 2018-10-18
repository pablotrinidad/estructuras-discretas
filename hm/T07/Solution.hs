s :: (Integral a) => a -> a
s n = n + 1

p :: (Integral a) => a -> a
p n = n - 1

g :: (Integral a) => a -> [a]
g 1 = [s 0]
g n = g(p(n)) ++ [n]


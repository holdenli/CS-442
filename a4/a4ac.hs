-- a4 part a c

lazymap :: (a -> b) -> [a] -> [b]
lazymap f (x:xs) = (f x):(lazymap f xs)


-- a5 part a 2

data State s a = ST(s -> (a, s))

instance Monad (State s) where
   return x = ST(\s -> (x, s))

   (ST x) >>= f = ST (\s0 -> let (a, s1) = x s0
                                 (ST g) = f a
                                 (b, s2) = g s1
                             in
                                 (b, s2)
                     )

type MyState = (Int, Int)

nextFib :: State (Int, Int) Int
nextFib = ST(\ (n, m) -> (n, (m, n+m)))

fiba 0 = return []
fiba n =
    do  ans1 <- nextFib
        ans2 <- fiba (n-1)
        return (ans1:ans2)

fib n = 
    let
        ST f = fiba n
    in
        fst (f (0, 1))


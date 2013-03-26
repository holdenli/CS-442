-- a5 part a 1

guess :: IO ()
guess =
    do  (putStr "Enter guessing range: ")
        s1 <- getLine
        guess2  (read (head (words s1)) :: Int)
                (read (last (words s1)) :: Int)
        putStr "\n"
    where
        guess2 :: Int -> Int -> IO ()
        guess2 x y =
            do  putStr ("Is it "++(show z)++"?: ")
                s1 <- getLine
                case s1 of
                    "higher" -> if z < y
                        then guess2 (z+1) y
                        else putStr "Cheating!"
                    "lower" -> if z > x
                        then guess2 x (z-1)
                        else putStr "Cheating!"
                    "yes" -> putStr "Got it!"
                    _ -> putStr "???"
            where
                z = quot (y - x) 2 + x


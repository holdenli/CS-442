(* a4 part a c *)

datatype 'a stream = Cons of 'a * (unit -> 'a stream) | Nil

fun lazymap f Nil = Nil
| lazymap f (Cons(a, b)) = Cons((f a), (fn () => (lazymap f (b ()))))


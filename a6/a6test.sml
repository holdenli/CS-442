(*

CS 442 Assignment 6
Holden Li - h55li - 20300403

*)

signature DFASIG = sig
    eqtype Q
    eqtype Sigma
    val init : Q
    val delta : Q * Sigma -> Q
    val accepting : Q list
end

signature NFASIG = sig
    eqtype Q
    eqtype Sigma
    val init : Q
    val delta : Q * Sigma -> Q list
    val accepting : Q list
end

functor NFA (Spec : NFASIG) : sig
    val run : Spec.Sigma list -> bool
end
= struct
    fun run l =
        let
            fun step [] _ = []
            |   step (x::xs) i =
                ( (Spec.delta (x, i)) @ (step xs i) )

            fun run' s [] = s
            |   run' s (x::xs) =
                run' (step s x) xs

            val final = run' [Spec.init] l

            fun member _ [] = false
            |   member x (y::ys) =
                x = y orelse member x ys

            fun member' [] _ = false
            |   member' (x::xs) y =
                (member x y) orelse (member' xs y)
        in
            member' final Spec.accepting
        end
end

structure NFATest = struct
    type Sigma = int
    type Q = int
    val init = 0
    val accepting = [7,8]

    fun delta (0, 0) = [0,1]
    |   delta (1, 1) = [7,8,11]
    |   delta (q, s) = [11]
end

structure TestInterp = NFA(NFATest)
val z = TestInterp.run [0,0];
val z = TestInterp.run [0,2];
val z = TestInterp.run [0,0,0,0,0,0,0,1];

functor DFAtoNFA (Spec : DFASIG) : NFASIG
= struct
    type Q = Spec.Q
    type Sigma = Spec.Sigma
    val init = Spec.init
    val accepting = Spec.accepting
    fun delta (q,s) = [(Spec.delta (q,s))]
end

functor Intersection (structure Spec1:DFASIG
    structure Spec2:DFASIG
    sharing type Spec1.Sigma = Spec2.Sigma) : DFASIG
= struct
    type Q = Spec1.Q * Spec2.Q
    type Sigma = Spec1.Sigma
    val init = (Spec1.init, Spec2.init)

    fun delta ((q1,q2), s) =
        ((Spec1.delta (q1,s)), Spec2.delta (q2,s))

    fun findAccepting [] _ = []
    |   findAccepting (x::xs) l = 
        let
            fun helper _ []  = []
            |   helper x (y::ys) = (x,y)::(helper x ys)
        in
            (helper x l) @ (findAccepting xs l)
        end
    val accepting = findAccepting Spec1.accepting Spec2.accepting
end

structure Odd1s = struct
    type Sigma = int
    datatype QAux = ODD | EVEN
    type Q = QAux
    val init = (EVEN)
    val accepting = [(ODD)]
    fun toggle ODD = EVEN
    | toggle _ = ODD

    fun delta(x ,1) = toggle x
    | delta(x, _) = x
end

structure Even0s = struct
    type Sigma = int
    datatype QAux = ODD | EVEN
    type Q = QAux
    val init = (EVEN)
    val accepting = [(EVEN)]

    fun toggle ODD = EVEN
    | toggle _ = ODD

    fun delta(x ,0) = toggle x
    | delta(x, _) = x
end

structure TEST2 = NFA(DFAtoNFA(Odd1s))
structure TEST3 = NFA(DFAtoNFA(Even0s))
structure Test = Intersection(
    structure Spec1 = Even0s
    structure Spec2 = Odd1s)
structure TEST4 = NFA(DFAtoNFA(Test))
val z = TEST2.run [1,0];
val z = TEST3.run [0,1,0];
val z = TEST4.run [0,1,0];
val z = TEST4.run [0,1,1,0];
val z = TEST4.run [1,1,0];

(* Discussion

The problem with our DFASIG that shows up if we try to compute the union
of two DFAs is with our representation of sigma. It is an eqtype which
does not correctly represent the alphabet of a DFA.

As seen in class, the alphabet for binary strings has sigma = int but
that includes other numbers besides 0 and 1. This is solved by adding
an exception to the DFA.

This ends up being cumbersome for complicated alphabets and impossible to
do when unioning two DFAs that accept different types for sigma (ie.
Spec1.sigma = int and Spec2.sigma = int list). This isn't possible
because we would need to have the new sigma be a new datatype
that can accomodate both sigmas. And in ML, there is no way to
do this cleanly because there is no implicit casting done on the input
of the DFA.

There are two solutions I can think of to this problem:

1.  Sigma is a function to test if input is in the alphabet.
    So delta must accept anything as input and test it against sigma.

2.  There is a function to cast input into the right type (sigma) for delta.

*)

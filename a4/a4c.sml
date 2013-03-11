(* a4 part c *)

type 'a cont = 'a SMLofNJ.Cont.cont
val callcc = SMLofNJ.Cont.callcc
val throw  = SMLofNJ.Cont.throw

fun scfoldr f i l =
    let
        fun hfoldr f i nil k = i
        |   hfoldr f i (x::rest) k =
            f x (hfoldr f i rest k) k
    in
        callcc (hfoldr f i l)
    end

fun exists e l =
    let
        fun hexists x y k =
            if x = e then throw k true
            else false
    in
        scfoldr hexists false l
    end


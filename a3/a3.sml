(*

CS 442 Assignment 3
Holden Li - h55li - 20300403

*)

datatype prim = Add | Neg | Mult | Div | And | Or | Not | Eq | Lt | Gt

fun prim_to_str Prim(Add)   = "add"
| prim_to_str Prim(Neg)     = "neg"
| prim_to_str Prim(Mult)    = "mult"
| prim_to_str Prim(Div)     = "div"
| prim_to_str Prim(And)     = "and"
| prim_to_str Prim(Or)      = "or"
| prim_to_str Prim(Not)     = "not"
| prim_to_str Prim(Eq)      = "eq"
| prim_to_str Prim(Lt)      = "lt"
| prim_to_str Prim(Gt)      = "gt"

datatype milner = Var of string
                | Abs of string * milner
                | App of milner * milner
                | If of milner * milner * milner
                | Let of string * milner * milner
                | Fix of string * milner
                | Int of int
                | Bool of bool
                | Prim of prim
                | Letex of string * milner
                | Raise of milner
                | Handle of milner * milner * milner

(* Instead of implementing polytypes, I'll just "mark" them. *)
datatype mtype = TInt | TBool | TVar of string
               | MTVar of string
               | TException
               | Arrow of mtype * mtype

(* Generating Type Variables:
   We reserve varables of the form Zn, n an integer, n>=0. *)
val counter = ref 0

fun newtype () = 
   "Z" ^ Int.toString(!counter before counter := !counter + 1)

(*  pptype
    Converts a mtype in a human readable string. *)
fun pptype (Arrow(x, y)) =
    "(" ^
    pptype x ^
    " -> " ^
    pptype y ^
    ")"
| pptype (MTVar(x)) = "'M." ^ x
| pptype (TVar(x)) = "'" ^ x
| pptype TInt = "int"
| pptype TBool = "bool"
| pptype TException = "exception"

(* Environment: mapping from names to types *)
type env = (string * mtype) list
val initenv = [
        ("add", Arrow(TInt, Arrow(TInt, TInt))),
        ("neg", Arrow(TInt, TInt)),
        ("mult",Arrow(TInt, Arrow(TInt, TInt))),
        ("div", Arrow(TInt, Arrow(TInt, TInt))),
        ("and", Arrow(TBool, Arrow(TBool, TBool))),
        ("or",  Arrow(TBool, Arrow(TBool, TBool))),
        ("not", Arrow(TBool, TBool)),
        ("eq",  Arrow(TInt, Arrow(TInt, TBool))),
        ("lt",  Arrow(TInt, Arrow(TInt, TBool))),
        ("gt",  Arrow(TInt, Arrow(TInt, TBool))),
        ("raise",  Arrow(TException, MTVar("raise"))),
        ("handle",  Arrow(
            TException,
            Arrow(
                MTVar("handle"),
                Arrow(MTVar("handle"), MTVar("handle"))
                )
            ))
    ]
val emptyenv = nil:env

(* subsitutions *)
fun sub (Arrow(x, y)) s = 
    (Arrow(sub x s, sub y s))
| sub (TVar(t)) (tau, TVar(alpha)) =
    if t = alpha then tau
    else TVar(t)
| sub t _ = t

fun mtype_sub t nil = t
| mtype_sub t (s::rest) = mtype_sub (sub t s) rest

fun env_sub (e:env) nil = e
| env_sub nil _ = nil
| env_sub ((x, t)::r1) (s::r2) =
    env_sub
        ((x, (sub t s))::r1) r2
    
(* unification *)
fun occurs_check (TVar(a1)) a2 =
    if a1 = a2 then true else false
| occurs_check (Arrow(x, y)) a =
    (occurs_check x a) andalso (occurs_check y a)
| occurs_check _ _ =
    false

exception CannotUnify
fun unify (Arrow(t1, t2)) (Arrow(t3, t4)) = 
    let
        val s1 = unify t1 t3
        val s2 = unify (mtype_sub t2 s1) (mtype_sub t4 s1)
    in
        s1 @ s2
    end
| unify (TVar(a)) t =
    if occurs_check t a then raise CannotUnify else [(t, TVar(a))]
| unify t (TVar(a)) =
    if occurs_check t a then raise CannotUnify else [(t, TVar(a))]
| unify (Arrow(_)) t = raise CannotUnify
| unify t (Arrow(_)) = raise CannotUnify
| unify t1 t2 =
    if t1 = t2 then [] else raise CannotUnify

(* Mark all variables in the type that are not free in the environment *)
fun mark (A:env) (t:mtype) =
    let

fun mtype_find_vars (TVar(x)) = [x]
| mtype_find_vars (Arrow(x, y)) = (mtype_find_vars x) @ (mtype_find_vars y)
| mtype_find_vars _ = []

fun env_find_vars (nil:env) = nil
| env_find_vars ((_, x)::rest) = (mtype_find_vars x) @ (env_find_vars rest)

fun rm_from_list nil _ = nil
| rm_from_list (i::rest) to_rm =
    if i = to_rm then
        rm_from_list rest to_rm
    else
        i::(rm_from_list rest to_rm)

fun rm_l_from_l nil _ = nil
| rm_l_from_l li nil = li
| rm_l_from_l li (r::rest) = rm_l_from_l (rm_from_list li r) rest

        val env_vars = env_find_vars A
        val t_vars = mtype_find_vars t
        val m_vars = rm_l_from_l t_vars env_vars

        fun mark_var (TVar(x)) s =
            if x = s then MTVar(x) else TVar(x)
        | mark_var (Arrow(x, y)) s =
            Arrow(mark_var x s, mark_var y s) 
        | mark_var t _ = t

        fun mark_vars (t:mtype) nil = t
        | mark_vars t (s::rest) = mark_vars (mark_var t s) rest
    in
        mark_vars t m_vars
    end

(* Generate a new variable for each marked variable *)
fun unmark t =
    let

fun find (MTVar(x)) = [x]
| find (Arrow(x, y)) = (find x) @ (find y)
| find _ = []

        val mvars = find t

        fun unmark_var (MTVar(x)) s new_var =
                if x = s then new_var else MTVar(x)
        | unmark_var (Arrow(x, y)) s n = Arrow(unmark_var x s n, unmark_var y s n)
        | unmark_var t _ _ = t
        
        fun unmark_vars (t:mtype) nil = t
        | unmark_vars t (s::rest) =
            let
                val new_var = TVar(newtype())
            in
                unmark_vars (unmark_var t s new_var) rest
            end
    in
        unmark_vars t mvars
    end

(* lookup *)
exception NotInEnv

fun lookup (nil:env) _ =
    raise NotInEnv
| lookup ((s, t)::rest) x =
    if x = s then
        unmark t
    else
        lookup rest x

(* W:  Accepts the arguments A (environment) and E (expression).
    Returns the type of E in A.  *)
fun W (A:env) (Var(x)) = (* Variables *)
    ([], (lookup A x))
| W A (Abs(x, y)) = (* Abstractions *)
    let
        val alpha = TVar(newtype())
        val (s, t) = W ((x, alpha)::A) y
    in
        (s, Arrow((mtype_sub alpha s), t))
    end
| W A (App(x, y)) = (* Applications *)
    let
        val alpha = TVar(newtype())
        val (s1, t1) = W A x
        val (s2, t2) = W (env_sub A s1) y
        val s3 = unify (mtype_sub t1 s2) (Arrow(t2, alpha))
    in
        (s1@s2@s3, (mtype_sub alpha s3))
    end
| W A (If(x, y, z)) = 
    let
        val (s1, t1) = W A x
        val s2 = unify t1 TBool
        val (s3, t3) = W (env_sub A (s1@s2)) y
        val (s4, t4) = W (env_sub A (s1@s2@s3)) z
        val s5 = unify (mtype_sub t3 s4) t4
    in
        (s1@s2@s3@s4@s5, t4)
    end
| W A (Let(x, y, z)) =
    let
        val (s1, t1) = W A y
        val marked_t1 = mark (env_sub A s1) t1
        val (s2, t2) = W ((x, marked_t1)::A) z
    in
        (s1@s2, t2)
    end
| W A (Fix(x, y)) =
    let
        val alpha = TVar(newtype())
        val (s1, t1) = W ((x, alpha)::A) y
        val s2 = unify (mtype_sub alpha s1) t1
    in
        (s1@s2, (mtype_sub t1 s2))
    end
| W A (Int(_)) =
    ([], TInt)
| W A (Bool(_)) =
    ([], TBool)
| W A (Prim(x)) = 
    W A (Var(prim_to_str Prim(x)))
| W A (Letex(x, y)) =
    let
        val (s, t) = W ((x, TException)::A) y
    in
        (s, t)
    end
| W A (Raise(x)) =
    W A (App(Var("raise"), x))
| W A (Handle(x, y, z)) =
    W A (App(App(App(Var("handle"), x), y), z))


(*  Starting point for CS 442/642 W13 Assignment 3

   These datatype declarations form an abstract syntax for Milner
   expressions.  All inputs to the type-inferencer are assumed to be
   syntactically valid Milner programs.  *)

datatype prim = Add | Neg | Mult | Div | And | Or | Not | Eq | Lt | Gt

datatype milner = Var of string
                | Abs of string * milner
                | App of milner * milner
                | If of milner * milner * milner
                | Let of string * milner * milner
                | Fix of string * milner
                | Int of int
                | Bool of bool
                | Prim of prim

datatype mtype = TInt | TBool | TVar of string
               | Arrow of mtype * mtype

(* Generating Type Variables:
   We reserve varables of the form Zn, n an integer, n>=0. *)
val counter = ref 0

fun newtype () = 
   "Z" ^ Int.toString(!counter before counter := !counter + 1)

fun newmtype (Arrow(x, y)) =
    Arrow(newmtype x, newmtype y)
| newmtype (TVar(t)) =
    TVar(newtype())
| newmtype t = t

(*  pptype
    Converts a mtype in a human readable string. *)
fun pptype (Arrow(x, y)) =
    "(" ^
    pptype x ^
    " -> " ^
    pptype y ^
    ")"
| pptype (TVar(x)) =
    "'" ^ x
| pptype TInt =
    "int"
| pptype TBool =
    "bool"

(* Environment: mapping from names to types *)

type mtype_pair = (mtype * mtype)
type env = (string * mtype) list
val initenv = nil:env
val emptyenv = nil:env

fun sub (Arrow(x, y)) (s:mtype_pair) = 
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

;
print "TEST 1\n";

sub TInt (TBool, TVar("a"));
sub (TVar "a") (TBool, TVar("a"));
sub (TVar "b") (TBool, TVar("a"));
sub (Arrow(TInt, TInt)) (TBool, TVar("a"));
sub (Arrow(TVar("a"), TVar("b"))) (TBool, TVar("a"));

print "TEST 2\n";

unify TInt TInt;
unify (TVar("a")) TInt;
unify (TVar("a")) (TVar("b"));
unify (Arrow(TInt, Arrow(TBool, TBool))) (Arrow(TVar("b"), TVar("a")));
unify
    (Arrow(TVar("a"), Arrow(TBool, TBool)))
    (Arrow(TInt, Arrow(TBool, TVar("a"))))
    handle CannotUnify => (print "CU: "; []);
unify TInt TBool handle CannotUnify => (print "CU: "; []);
unify TInt TBool handle CannotUnify => (print "CU: "; []);

(* **************************************** *)
print "#### W\n";

exception NotInEnv
fun lookup (nil:env) _ =
    raise NotInEnv
| lookup ((s, t)::rest) x =
    if x = s then
        t
    else
        lookup rest x

exception NotImplemented

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
        val alpha = TVar(newtype())
    in
        ([], alpha)
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
| W A E =
    raise NotImplemented  

;
print "#### TEST\n";

W initenv (Var("a")) handle NotInEnv => (print "OHNO: "; ([], TInt));
W [("a", TInt)] (Var("a"));
W initenv (Abs("a", Var("a")));
W initenv (App(Int(1), Int(1))) handle CannotUnify => (print "OHNO: "; ([], TInt));
W initenv (App(Abs("a", Var("a")), Int(1)));
W initenv
    (If(
        Bool(true),
        Int(1),
        Int(2)
    ));


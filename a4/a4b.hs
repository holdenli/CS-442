--  a4 part b

data Primitive = Succ | IsZero

data Term = Var String | Abs String Term | App Term Term
          | Prim Primitive | INT Int

trueTerm = Abs "x" (Abs "y" (Var "x"))
falseTerm = Abs "x" (Abs "y" (Var "y"))

instance Show Term where
    show (Var x) = x
    show (Abs s t) = "(\\"++s++".("++(show t)++"))"
    show (App x y) = "("++(show x)++" "++(show y)++")"
    show (Prim Succ) = "Succ"
    show (Prim IsZero) = "IsZero"
    show (INT i) = show i

applyPrim :: Primitive -> Term -> Term
applyPrim Succ (INT i) = (INT (i + 1))
applyPrim IsZero (INT i)
    | i == 0 = trueTerm
    | otherwise = falseTerm
applePrim IsZero _ = falseTerm

-- ##############################

data SContents = S Term  | SClo (String, Term, [EContents])
data EContents = E (String, SContents)
data CContents = C Term | CApply
data DContents = D ([SContents], [EContents], [CContents])
data SECDConfig = SECD ([SContents], [EContents], [CContents], [DContents])
    | SECDDone (SContents)

lookUp :: String -> [EContents] -> SContents
lookUp s ((E(x,t)):xs)
    | x == s    = t
    | otherwise = lookUp s xs

-- ##############################

secdOneStep :: SECDConfig -> SECDConfig
secdOneStep (SECD (s, e, C(Var x):c, d))    = SECD ((lookUp x e):s, e, c, d)
secdOneStep (SECD (s, e, C(Abs x m):c, d))  = SECD (SClo(x, m, e):s, e, c, d)
secdOneStep (SECD (s, e, C(App m n):c, d))  = SECD (s, e, C(n):C(m):CApply:c, d)
secdOneStep (SECD (s, e, C(INT i):c, d))    = SECD (S(INT i):s, e, c, d)
secdOneStep (SECD (s, e, C(Prim p):c, d))   = SECD (S(Prim p):s, e, c, d)
secdOneStep (SECD (S(Prim p):S(n):s, e, CApply:c, d)) =
    SECD (s, e, C(applyPrim p n):c, d)
secdOneStep (SECD (SClo(x, m, e1):n:s, e, CApply:c, d)) =
    SECD ([],
        E(x, n):e1,
        [C(m)],
        D(s, e, c):d)
secdOneStep (SECD (m:[], e1, [], D(s, e, c):d)) = SECD (m:s, e, c, d)
secdOneStep (SECD (m:[], e, [], []))            = SECDDone(m)
secdOneStep (SECDDone(x)) = SECDDone(x)

clojureToTerm :: SContents -> Term
clojureToTerm (S (t)) = t
clojureToTerm (SClo (s, t, e)) = Abs s (applyEnv t e)

applyEnv :: Term -> [EContents] -> Term
applyEnv t [] = t
applyEnv (Var s) (E(e, t):es)
    | s == e = clojureToTerm t
    | otherwise = Var s
applyEnv (INT i) _ = (INT i)
applyEnv (Prim p) _ = (Prim p)
applyEnv (App x y) e = App (applyEnv x e) (applyEnv y e)

-- ##############################

reduceD  :: Term -> SECDConfig
reduceD t =
    reduceH (SECD ([], [], [C(t)], []))
    where
        reduceH :: SECDConfig -> SECDConfig
        reduceH (SECD (s, e, c, d)) = reduceH (secdOneStep (SECD (s, e, c, d)))
        reduceH z = z

reduce  :: Term -> Term
reduce t =
    reduceH (SECD ([], [], [C(t)], []))
    where
        reduceH :: SECDConfig -> Term
        reduceH (SECDDone (S (t))) = t
        reduceH (SECDDone (SClo (s, t, e))) = clojureToTerm (SClo (s, t, e))
        reduceH (SECD (s, e, c, d)) = reduceH (secdOneStep (SECD (s, e, c, d)))

-- ##############################

instance Show SContents where
    show (S (t)) = (show t)
    show (SClo (s, t, e)) = "<"++s++", "++(show t)++", "++(show e)++">"

instance Show EContents where
    show (E (s, t)) = "("++s++", "++(show t)++")"

instance Show CContents where
    show (C (t)) = show t
    show CApply = "@"

instance Show DContents where
    show (D(s, e, c)) = "<"++(show s)++", "++(show e)++", "++(show c)++">"

instance Show SECDConfig where
    show (SECD (s, e, c, d)) =
        "S = "++(show s)++"\n"++
        "E = "++(show e)++"\n"++
        "C = "++(show c)++"\n"++
        "D = "++(show d)++""
    show (SECDDone(x)) = "Done: "++show x

-- TEST CASES

a = App (Abs "x" (Abs "y" (App (Var "x") (Var "y")))) falseTerm
b = App (App a (INT 1)) (INT 2)
c = (App (Abs "x" (Var "x")) (INT 1))
d = App (Prim IsZero) (INT 0)
e = (Prim Succ)
f = App (Abs "x" (Abs "y" (Var "x"))) (Abs "z" (Var "z")) 

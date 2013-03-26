/* CS442 A5
 * Holden Li - h55li
 *****************************************************************************/

/* PART B
 *****************************************************************************/
interpret([], []).

/* Apply Failure */
interpret([a, _|[]], _) :- fail, !.

/* Delay Stuff */
interpret([a, d, a, a|T], Y) :-
    interpret([a|T], X),
    interpret([a,d,a|X], Y),
    !.
interpret([a, d, a, O, a|T], Y) :-
    interpret([a|T], X),
    interpret([a,d,a,O|X], Y),
    !.
interpret([a, d, a, X, Y|T],   [promise([a, X, Y])|T]) :- !.
interpret([a, d, F|T],         [promise([F])|T]) :- !.

/* Reduce tails */
interpret([a, a|T], Y)     :- interpret([a|T], X), interpret([a|X], Y), !.
interpret([a, O, a|T], Y)  :- interpret([a|T], X), interpret([a, O|X], Y), !.

/* Other stuff */
interpret([a, k, X|T],         [k(X)|T]) :- !.
interpret([a, k(X), _|T],      [X|T]) :- !.
interpret([a, s, X|T],         [s(X)|T]) :- !.
interpret([a, s(X), Y|T],      [s(X,Y)|T]) :- !.
interpret([a, s(X,Y), Z|T],    A) :- interpret([a,a,X,Z,a,Y,Z|T], A), !.
interpret([a, i, X|T],         [X|T]) :- !.
interpret([a, v, _|T],         [v|T]) :- !.
interpret([a, dot(X), Y|T],    [Y|T]) :- write(X), !.
interpret([a, r, Y|T],         [Y|T]) :- nl, !.

interpret([a, promise(L)|T],   B) :- append(L, T, A), interpret(A, B), !.

/* PART C
 *****************************************************************************/
textToUnlambda("", []).
textToUnlambda([96|T], [a|A]) :-
    textToUnlambda(T, A), !.
textToUnlambda([46,H|T], [dot(X)|A]) :-
    name(X, [H]), textToUnlambda(T, A), !.
textToUnlambda([H|T], [X|A]) :-
    name(X,[H]), textToUnlambda(T, A), !.
interpretFromText(S, Y) :- textToUnlambda(S, X), interpret(X, Y).

/* PART D
 *****************************************************************************/

babz(X,[var(X)],I) :- iii(I), !.
babz(_,[var(Y)],app(K, var(Y))) :- kkk(K), !.
babz(_,[func(Y)],app(K, func(Y))) :- kkk(K), !.

unlambda(var(X),X).
unlambda(func(F),F).
unlambda(app(M,N),app(X,Y)) :- unlambda(M,X), unlambda(N,Y).
unlambda(abs(var(X),E),Z) :- unlambda(E,EE), babz(X,EE,Z).

iii(I) :- I = abs(var(x), var(x)).

kkk(K) :- K = abs(var(x), abs(var(y), var(x))).

sss(S) :-
    S =
    abs(var(x), abs(var(y), abs(var(z),
    app(
        app(var(x), var(z)),
        app(var(y), var(z))
        )))).

babs(X,[X],[i]) :- write(99),nl,!.
babs(_,[Y],[a,k,Y]) :- write(999),nl,!.
/*
babs(X, [a,a|T], [a,Z|T]) :- 
    write(1),write(X),nl,
    write([a,a|T]),nl,
    babs(X, [a|T], Z),
    !.
babs(X, [a,O,a|T], [a,O,Z|T]) :-
    write(2),write(X),nl,
    write([a,O,a|T]),nl,
    babs(X, [a|T], Z),!.
babs(X, [a,A,B], [a, s, a|Z]) :-
    write(3),write(X),nl,
    write([a,A,B]),nl,
    babs(X, [A], M),
    babs(X, [B], N),
    append(M,N,Z),
    write(Z),nl,
    !.
*/
babs(X, [a|T], [a,a,s|Z]) :-
    write(3),write(X),nl,
    getF(T,F,_),
    getS(T,S,_),
    write([a|T]),nl,
    write([F,S]),nl,
    babs(X, F, M),
    babs(X, S, N),
    append(M,N,Z),
    write(Z),nl,
    !.

getF([a], _, _) :- fail, !.
getF([a,_], _, _) :- fail, !.
getF([a|T], [a|Z], R) :-
    getF(T, F, _),
    getS(T, S, R),
    append(F,S,Z),
    !.
getF([X|R], [X], R).

getS([a], _, _) :- fail, !.
getS([a,_], _, _) :- fail, !.
getS([a|T], A, R) :-
    getF(T, _, TT),
    getF(TT, _, TTT),
    getF(TTT, A, R),
    !.
getS([_,a|T], [a|Z], R) :-
    getF(T, F, _),
    getS(T, S, R),
    append(F,S,Z),
    !.
getS([A,X|R], [X], R) :- A \= a.

unlambdafy(var(X), [X]).
unlambdafy(func(dot(X)), [dot(Y)]) :-
    /* Convert string to name for interpret input */
    name(Y,X), !. 
unlambdafy(func(X), [X]).
unlambdafy(app(M,N), [a|A]) :- unlambdafy(M,X), unlambdafy(N,Y), append(X,Y,A).
unlambdafy(abs(var(X),E), Y) :-
    unlambdafy(E,EE),
    write(123456),nl,
    write(EE),nl,
    babs(X, EE, Y).

/* Unlambdafy and Interpret - for testing purposes */
chain(A,B) :- unlambdafy(A,X), interpret(X,B).


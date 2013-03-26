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

/* Get first arg */
getF([a], _, _) :- fail, !.
getF([a,_], _, _) :- fail, !.
getF([a|T], [a|Z], R) :-
    getF(T, F, _),
    getS(T, S, R),
    append(F,S,Z),
    !.
getF([X|R], [X], R).

/* Get second arg */
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

/* bracket abs */
babs(X,[X],[i]) :- !.
babs(_,[Y],[a,k,Y]) :- !.
babs(X, [a|T], [a,a,s|Z]) :-
    getF(T,F,_),
    getS(T,S,_),
    babs(X, F, M),
    babs(X, S, N),
    append(M,N,Z),
    !.

unlambdafy(var(X), [X]).
unlambdafy(func(dot(X)), [dot(Y)]) :-
    /* Convert string to name for interpret input */
    name(Y,X), !. 
unlambdafy(func(X), [X]).
unlambdafy(app(M,N), [a|A]) :- unlambdafy(M,X), unlambdafy(N,Y), append(X,Y,A).
unlambdafy(abs(var(X),E), Y) :-
    unlambdafy(E,EE),
    babs(X, EE, Y).

/* Unlambdafy and Interpret - for testing purposes */
chain(A,B) :- unlambdafy(A,X), interpret(X,B).


parties([1,2,3,4,5,6,7,8]).
mandate([38,17,51,23,27,35,18,25]).

inCommon([
    [4,-8,8,1,4,10,-3,-2],
    [-1,-9,9,9,1,4,-2,2],
    [4,1,9,4,-4,-10,0,-10],
    [0,3,-1,-4,1,-6,6,2],
    [5,-2,6,4,-7,-10,1,6],
    [4,6,-2,-4,-3,9,-5,1],
    [8,8,-2,-2,-4,-5,-10,7],
    [-4,-4,6,-1,4,10,9,4]
]).

permute([], []).
permute([H | T], F) :-
    permute(T, F1),
    append(F2, F3, F1),
    append(F2, [H | F3], F).

partlist(X, F) :-
    permute(X, X1),
    append(_, F1, X1),
    append(F2, _, F1),
    F = F2.

nth(N,L,E) :- nth(1,N,L,E).
nth(N,N,[H|_],H).
nth(K,N,[_|T],H) :- K1 is K+1, nth(K1,N,T,H).

sumMandate([], 0).
sumMandate([H | T], N) :-
    sumMandate(T, N1),
    nth(H, mandate(_), E),
    N is N1 + E.

stableGovernment(G) :-
    parties(P),
    partlist(P, G),
    sumMandate(G, S),
    S >= 117. /*117 is half the number of mandates*/


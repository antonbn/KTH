findpath(X, Y, P) :- findpath(X, Y, [], P).
findpath(X, Y, Q, P) :-
    edge(X, Y),
    append(Q, [X, Y], P).
findpath(X, Y, Q, P) :-
    edge(X, Z),
    \+ member(Z, Q),
    findpath(Z, Y, [X | Q], P).

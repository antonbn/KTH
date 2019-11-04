partstring(X, F) :-
    append(_, F1, X),
    append(F2, _, F1),
    F = F2.

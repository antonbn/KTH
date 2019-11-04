permute([], []).
permute([H | T], F) :-
    permute(T, F1),
    append(F2, F3, F1),
    append(F2, [H | F3], F).

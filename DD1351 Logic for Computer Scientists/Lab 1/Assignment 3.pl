findlast([E], [], E).
findlast([H | T], [H | Y], E) :- findlast(T, Y,  E).

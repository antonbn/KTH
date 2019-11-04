% Load model, initial state and formula from file.
verify(Input) :-
    see(Input), read(T), read(L), read(S), read(F), seen,
    check(T, L, S, [], F).
% check(T, L, S, U, F)
% T - The transitions in form of adjacency lists
% L - The labeling
% S - Current state
% U - Currently recorded states
% F - CTL Formula to check.
%
% Should evaluate to true iff the sequent below is valid.
%
% (T,L), S |- F
%            U

find_list([[S, L] | _], S, L).
find_list([_ | T], Current, List):-
    find_list(T, Current, List).

% Phi finns i tillståndet
check(_, Labeling, Current, _, Phi) :-
    find_list(Labeling, Current, Labels),
    member(Phi, Labels).
% Phi finns inte i tillståndet
check(AdjList, Labeling, Current, _, neg(Phi)) :-
    not(check(AdjList, Labeling, Current, [], Phi)).
% Phi och Psi finns i tillståndet
check(AdjList, Labeling, Current, _, and(Phi, Psi)) :-
    check(AdjList, Labeling, Current, [], Phi),
    check(AdjList, Labeling, Current, [], Psi).
% Phi finns i tillståndet
check(AdjList, Labeling, Current, _, or(Phi, _)) :-
    check(AdjList, Labeling, Current, [], Phi).
% eller så finns Psi i tillståndet
check(AdjList, Labeling, Current, _, or(_, Psi)) :-
    check(AdjList, Labeling, Current, [], Psi).
% Phi finns med i alla nästa tillstånd från S
check(AdjList, Labeling, Current, _, ax(Phi)) :-
    find_list(AdjList, Current, Neighbours),
    check_all_neighbours(AdjList, Labeling, Neighbours, [], Phi).
% Alltid Phi i alla vägar
check(_, _, Current, Visited, ag(_)) :-
    member(Current, Visited).
check(AdjList, Labeling, Current, Visited, ag(Phi)) :-
    not(member(Current, Visited)),
    check(AdjList, Labeling, Current, [], Phi),
    find_list(AdjList, Current, Neighbours),
    check_all_neighbours(AdjList, Labeling, Neighbours, [Current | Visited], ag(Phi)).
% I alla vägar kommer ett tillstånd ha Phi
check(AdjList, Labeling, Current, Visited, af(Phi)) :-
    not(member(Current, Visited)),
    check(AdjList, Labeling, Current, [], Phi).
check(AdjList, Labeling, Current, Visited, af(Phi)) :-
    not(member(Current, Visited)),
    find_list(AdjList, Current, Neighbours),
    check_all_neighbours(AdjList, Labeling, Neighbours, [Current | Visited], af(Phi)).
% Phi finns med i något nästa tillstånd från S
check(AdjList, Labeling, Current, _, ex(Phi)) :-
    find_list(AdjList, Current, Neighbours),
    not(check_all_neighbours(AdjList, Labeling, Neighbours, [], neg(Phi))).
% Det finns en väg där alltid Phi
check(_, _, Current, Visited, eg(_)) :-
    member(Current, Visited).
check(AdjList, Labeling, Current, Visited, eg(Phi)) :-
    not(member(Current, Visited)),
    check(AdjList, Labeling, Current, [], Phi),
    find_list(AdjList, Current, Neighbours),
    check_some_neighbour(AdjList, Labeling, Neighbours, [Current | Visited], eg(Phi)).
% I någon väg kommer ett tillstånd ha Phi
check(AdjList, Labeling, Current, Visited, ef(Phi)) :-
    not(member(Current, Visited)),
    check(AdjList, Labeling, Current, [], Phi).
check(AdjList, Labeling, Current, Visited, ef(Phi)) :-
    not(member(Current, Visited)),
    find_list(AdjList, Current, Neighbours),
    check_some_neighbour(AdjList, Labeling, Neighbours, [Current | Visited], ef(Phi)).

check_some_neighbour(AdjList, Labeling, [NeighbourHead | _], Visited, Phi) :-
    check(AdjList, Labeling, NeighbourHead, Visited, Phi).
check_some_neighbour(AdjList, Labeling, [_ | NeighbourTail], Visited, Phi) :-
    check_some_neighbour(AdjList, Labeling, NeighbourTail, Visited, Phi).

check_all_neighbours(_, _, [], _, _).
check_all_neighbours(AdjList, Labeling, [NeighbourHead | NeighbourTail], Visited, Phi) :-
    check(AdjList, Labeling, NeighbourHead, Visited, Phi),
    check_all_neighbours(AdjList, Labeling, NeighbourTail, Visited, Phi).

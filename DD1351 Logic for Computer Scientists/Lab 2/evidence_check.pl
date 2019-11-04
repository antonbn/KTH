% Läser in inputfilen och kör valid_proof med indatan.
verify(InputFileName) :-
    see(InputFileName),
    read(Prems), read(Goal), read(Proof),
    seen,
    valid_proof(Prems, Goal, Proof, Proof).

verify(Prems, Goal, Proof) :- valid_proof(Prems, Goal, Proof, Proof).

% Basfall, huvudet på listan har samma radnummer som det sökta,
% Expression och Rule ska då vara samma som på den funna raden.
get_row(RowNr, [[RowNr, Expression, Rule] | _], Expression, Rule).
% Huvudet på listan är en rad, vi kör rekursivt get_row med
% de rader som kommer efteråt.
get_row(RowNr, [[_, _ , _] | Proof], Expression, Rule) :-
    get_row(RowNr, Proof, Expression, Rule).
% Huvudet på listan är en box, vi kör då rekursivt get_row med boxen
% för att kolla alla rader i boxen.
get_row(RowNr, [[Row | Box] | _], Expression, Rule) :-
    get_row(RowNr, [Row | Box], Expression, Rule).
% Huvudet på listan är en box, vi hoppar över boxen och kör rekursivt
% get_row med de rader som kommer efter boxen. Detta anrop tillsammans
% med det ovanstående ser till att alla rader kan kommas åt.
get_row(RowNr, [[[_, _, _] | _] | Proof], Expression, Rule) :-
    get_row(RowNr, Proof, Expression, Rule).

% Expression ska vara detsamma som på rad X.
check_rule(_, [RowNr, Expression, copy(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, Expression, Rule),
    not(Rule = box(_)).
% Formatet ska vara (Expression på rad X) && (Expression på rad Y).
check_rule(_, [RowNr, and(Expression1, Expression2), andint(X, Y)], Proof) :-
    X < RowNr, Y < RowNr,
    get_row(X, Proof, Expression1, Rule1),
    not(Rule1 = box(_)),
    get_row(Y, Proof, Expression2, Rule2),
    not(Rule2 = box(_)).
% Ska vara det uttryck som är till vänster om och-tecknet på rad X.
check_rule(_, [RowNr, Expression, andel1(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, and(Expression, _), Rule),
    not(Rule = box(_)).
% Ska vara det uttrcyk som är till höger om och-tecknet på rad X.
check_rule(_, [RowNr, Expression, andel2(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, and(_, Expression), Rule),
    not(Rule = box(_)).
% Ska vara det uttrcyk på rad X med två negationstecken bortagna.
check_rule(_, [RowNr, Expression, negnegel(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, neg(neg(Expression)), Rule),
    not(Rule = box(_)).
% Expression2 då rad X är Expression1 samt rad Y är (Expression1 -> Expression2).
check_rule(_, [RowNr, Expression2, impel(X, Y)], Proof) :-
    X < RowNr, Y < RowNr,
    get_row(X, Proof, Expression1, Rule1),
    not(Rule1 = box(_)),
    get_row(Y, Proof, imp(Expression1, Expression2), Rule2),
    not(Rule2 = box(_)).
% Rad X ska inehålla Expression medan rad Y innehåller icke Expression.
check_rule(_, [RowNr, cont, negel(X, Y)], Proof) :-
    X < RowNr, Y < RowNr,
    get_row(X, Proof, Expression, Rule1),
    not(Rule1 = box(_)),
    get_row(Y, Proof, neg(Expression), Rule2),
    not(Rule2 = box(_)).
% Rad X ska vi anta Expression och rad Y en motsägelse ger neg(Expression).
check_rule(_, [RowNr, neg(Expression), negint(X, Y)], Proof) :-
    X < RowNr, Y < RowNr, X =< Y,
    get_row(X, Proof, Expression, box(X)),
    get_row(Y, Proof, cont, box(X)).
% Första delen av eller-satsen ska finnas på rad X.
check_rule(_, [RowNr, or(Expression, _), orint1(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, Expression, Rule),
    not(Rule = box(_)).
% Andra delen av eller-satsen ska finnas på rad X.
check_rule(_, [RowNr, or(_, Expression), orint2(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, Expression, Rule),
    not(Rule = box(_)).
%  Rad Y & V är antaganden för vardera del av eller-satsen på rad X,
%  båda antaganden leder fram till Expression på rad U & W.
check_rule(_, [RowNr, Expression, orel(X, Y, U, V, W)], Proof) :-
    X < RowNr, Y < RowNr, U < RowNr, V < RowNr, W < RowNr, Y =< U, V =< W,
    get_row(X, Proof, or(Expression1, Expression2), Rule),
    not(Rule = box(_)),
    get_row(Y, Proof, Expression1, box(Y)),
    get_row(U, Proof, Expression, box(Y)),
    get_row(V, Proof, Expression2, box(V)),
    get_row(W, Proof, Expression, box(V)).
% Om vi antar Expression1 och kommer fram till Expression2 så gäller
% Expression1 -> Expression2.
check_rule(_, [RowNr, imp(Expression1, Expression2), impint(X, Y)], Proof) :-
    X < RowNr, Y < RowNr, X =< Y,
    get_row(X, Proof, Expression1, box(X)),
    get_row(Y, Proof, Expression2, box(X)).
%  Om vi kommit fram till en motsägelse kan vi säga att vad som helst
%  stämmer.
check_rule(_, [RowNr, _, contel(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, cont, Rule),
    not(Rule = box(_)).
% Om vi har Expression kan vi säga att neg(neg(Expression)) stämmer.
check_rule(_, [RowNr, neg(neg(Expression)), negnegint(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, Expression, Rule),
    not(Rule = box(_)).
% Om neg(Expression2) och Expression1 -> Expression2 gäller
% neg(Expression1).
check_rule(_, [RowNr, neg(Expression1), mt(X, Y)], Proof) :-
    X < RowNr, Y < RowNr,
    get_row(X, Proof, imp(Expression1, Expression2), Rule1),
    not(Rule1 = box(_)),
    get_row(Y, Proof, neg(Expression2), Rule2),
    not(Rule2 = box(_)).
% Rad X ska vi anta neg(Expression) och rad Y en motsägelse ger
% Expression.
check_rule(_, [RowNr, Expression, pbc(X, Y)], Proof) :-
    X < RowNr, Y < RowNr, X =< Y,
    get_row(X, Proof, neg(Expression), box(X)),
    get_row(Y, Proof, cont, box(X)).
% Kan få Expression || neg(Expression) från ingenting.
check_rule(_, [_, or(Expression, neg(Expression)), lem], _).
% Antagande utanför boxöppning bör alltid vara falskt.
check_rule(_, [_, _, assumption], _) :- fail.
% Är Expression en faktiskt premiss?
check_rule(Prems, [_, Expression, premise], _) :-
    member(Expression, Prems).

% Basfall, vi är i slutet av beviset. Vi validerar regeln och kollar
% sedan om Expression är detsamma som målet.
valid_proof(Prems, Goal, [[RowNr, Expression, Rule]], Proof) :-
    last(Proof, [RowNr, Expression, Rule]),
    !,
    check_rule(Prems, [RowNr, Expression, Rule], Proof),
    Expression = Goal.
% Vi är i slutet av en box. Vi validerar regeln.
valid_proof(Prems, _, [[RowNr, Expression, Rule]], Proof) :-
    !,
    check_rule(Prems, [RowNr, Expression, Rule], Proof).
% En box öppnas, första raden måste vara ett antagande.
% Vi går rekursivt igenom hela boxen
% Vi "tar sedan bort" boxen
% Vi går igenom resten av beviset
valid_proof(Prems, Goal, [[[RowNr, _, assumption] | Box] | FollowingProof], Proof) :-
    valid_proof(Prems, Goal, Box, Proof),
    remove_box(RowNr, Proof, NewProof),
    valid_proof(Prems, Goal, FollowingProof, NewProof).
% En box öppnas, finns bara en rad i den.
% Första raden är en assumption och behöver inte valideras.
% Vi "tar bort" boxen och går rekursivt igenom resten av beviset.
valid_proof(Prems, Goal, [[[RowNr, _, assumption]] | FollowingProof], Proof) :-
    remove_box(RowNr, Proof, NewProof),
    valid_proof(Prems, Goal, FollowingProof, NewProof).
% Vi kollar att raden stämmer och går sedan till nästa rad.
valid_proof(Prems, Goal, [Row | FollowingProof], Proof) :-
    !,
    check_rule(Prems, Row, Proof),
    valid_proof(Prems, Goal, FollowingProof, Proof).

% remove_box(Radnummer, Bevis med box, Bevis utan box).
/*RowNr är på den första raden i boxen. Vi sätter "Bevis utan box" att
 vara en lista som innehåller två rader: rad 1 i boxen men med regeln
 box(RowNr), och rad 2 men med regeln box(RowNr). Sedan sätter vi
 kriteriumet att rad 2 är den sista raden i "Bevis med box". */
remove_box(RowNr, [[RowNr, Expression, assumption] | Box], [[RowNr, Expression, box(RowNr)], [RowNr2, Expression2, box(RowNr)]]) :-
    last(Box, [RowNr2, Expression2, _]).
% "Bevis med box" innehåller enbart en rad, "Bevis utan box" ska då vara
% denna rad men med regeln box(RowNr).
remove_box(RowNr, [[RowNr, Expression, assumption]], [[RowNr, Expression, box(RowNr)]]).
% Vi har funnit en inre box. Huvudet på "Bevis med box" är en box,
% huvudet på "Bevis utan box" ska då också vara en box. Vi kör
% remove_box rekursivt med dessa boxar.
remove_box(RowNr, [[Row | Box] | Proof], [[NewRow | NewBox] | Proof]) :-
    remove_box(RowNr, [Row | Box], [NewRow | NewBox]).
% Huvudet på "Bevis med box" är en rad eller box, huvudet på "Bevis utan
% box" ska vara detsamma. Vi kör remove_box rekursivt med de rader som
% kommer efter den nuvarande raden. (Om vi med denna metod råkar hoppa
% över boxen som vi ska till och komma till slutet av beviset så kommer
% metoden att fallera. Då kommer den backtracka och köra metoden över
% och gå in i boxen.)
remove_box(RowNr, [Row | Proof], [Row | NewProof]) :-
    remove_box(RowNr, Proof, NewProof).

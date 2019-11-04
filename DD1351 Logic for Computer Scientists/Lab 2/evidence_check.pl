% L�ser in inputfilen och k�r valid_proof med indatan.
verify(InputFileName) :-
    see(InputFileName),
    read(Prems), read(Goal), read(Proof),
    seen,
    valid_proof(Prems, Goal, Proof, Proof).

verify(Prems, Goal, Proof) :- valid_proof(Prems, Goal, Proof, Proof).

% Basfall, huvudet p� listan har samma radnummer som det s�kta,
% Expression och Rule ska d� vara samma som p� den funna raden.
get_row(RowNr, [[RowNr, Expression, Rule] | _], Expression, Rule).
% Huvudet p� listan �r en rad, vi k�r rekursivt get_row med
% de rader som kommer efter�t.
get_row(RowNr, [[_, _ , _] | Proof], Expression, Rule) :-
    get_row(RowNr, Proof, Expression, Rule).
% Huvudet p� listan �r en box, vi k�r d� rekursivt get_row med boxen
% f�r att kolla alla rader i boxen.
get_row(RowNr, [[Row | Box] | _], Expression, Rule) :-
    get_row(RowNr, [Row | Box], Expression, Rule).
% Huvudet p� listan �r en box, vi hoppar �ver boxen och k�r rekursivt
% get_row med de rader som kommer efter boxen. Detta anrop tillsammans
% med det ovanst�ende ser till att alla rader kan kommas �t.
get_row(RowNr, [[[_, _, _] | _] | Proof], Expression, Rule) :-
    get_row(RowNr, Proof, Expression, Rule).

% Expression ska vara detsamma som p� rad X.
check_rule(_, [RowNr, Expression, copy(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, Expression, Rule),
    not(Rule = box(_)).
% Formatet ska vara (Expression p� rad X) && (Expression p� rad Y).
check_rule(_, [RowNr, and(Expression1, Expression2), andint(X, Y)], Proof) :-
    X < RowNr, Y < RowNr,
    get_row(X, Proof, Expression1, Rule1),
    not(Rule1 = box(_)),
    get_row(Y, Proof, Expression2, Rule2),
    not(Rule2 = box(_)).
% Ska vara det uttryck som �r till v�nster om och-tecknet p� rad X.
check_rule(_, [RowNr, Expression, andel1(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, and(Expression, _), Rule),
    not(Rule = box(_)).
% Ska vara det uttrcyk som �r till h�ger om och-tecknet p� rad X.
check_rule(_, [RowNr, Expression, andel2(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, and(_, Expression), Rule),
    not(Rule = box(_)).
% Ska vara det uttrcyk p� rad X med tv� negationstecken bortagna.
check_rule(_, [RowNr, Expression, negnegel(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, neg(neg(Expression)), Rule),
    not(Rule = box(_)).
% Expression2 d� rad X �r Expression1 samt rad Y �r (Expression1 -> Expression2).
check_rule(_, [RowNr, Expression2, impel(X, Y)], Proof) :-
    X < RowNr, Y < RowNr,
    get_row(X, Proof, Expression1, Rule1),
    not(Rule1 = box(_)),
    get_row(Y, Proof, imp(Expression1, Expression2), Rule2),
    not(Rule2 = box(_)).
% Rad X ska ineh�lla Expression medan rad Y inneh�ller icke Expression.
check_rule(_, [RowNr, cont, negel(X, Y)], Proof) :-
    X < RowNr, Y < RowNr,
    get_row(X, Proof, Expression, Rule1),
    not(Rule1 = box(_)),
    get_row(Y, Proof, neg(Expression), Rule2),
    not(Rule2 = box(_)).
% Rad X ska vi anta Expression och rad Y en mots�gelse ger neg(Expression).
check_rule(_, [RowNr, neg(Expression), negint(X, Y)], Proof) :-
    X < RowNr, Y < RowNr, X =< Y,
    get_row(X, Proof, Expression, box(X)),
    get_row(Y, Proof, cont, box(X)).
% F�rsta delen av eller-satsen ska finnas p� rad X.
check_rule(_, [RowNr, or(Expression, _), orint1(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, Expression, Rule),
    not(Rule = box(_)).
% Andra delen av eller-satsen ska finnas p� rad X.
check_rule(_, [RowNr, or(_, Expression), orint2(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, Expression, Rule),
    not(Rule = box(_)).
%  Rad Y & V �r antaganden f�r vardera del av eller-satsen p� rad X,
%  b�da antaganden leder fram till Expression p� rad U & W.
check_rule(_, [RowNr, Expression, orel(X, Y, U, V, W)], Proof) :-
    X < RowNr, Y < RowNr, U < RowNr, V < RowNr, W < RowNr, Y =< U, V =< W,
    get_row(X, Proof, or(Expression1, Expression2), Rule),
    not(Rule = box(_)),
    get_row(Y, Proof, Expression1, box(Y)),
    get_row(U, Proof, Expression, box(Y)),
    get_row(V, Proof, Expression2, box(V)),
    get_row(W, Proof, Expression, box(V)).
% Om vi antar Expression1 och kommer fram till Expression2 s� g�ller
% Expression1 -> Expression2.
check_rule(_, [RowNr, imp(Expression1, Expression2), impint(X, Y)], Proof) :-
    X < RowNr, Y < RowNr, X =< Y,
    get_row(X, Proof, Expression1, box(X)),
    get_row(Y, Proof, Expression2, box(X)).
%  Om vi kommit fram till en mots�gelse kan vi s�ga att vad som helst
%  st�mmer.
check_rule(_, [RowNr, _, contel(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, cont, Rule),
    not(Rule = box(_)).
% Om vi har Expression kan vi s�ga att neg(neg(Expression)) st�mmer.
check_rule(_, [RowNr, neg(neg(Expression)), negnegint(X)], Proof) :-
    X < RowNr,
    get_row(X, Proof, Expression, Rule),
    not(Rule = box(_)).
% Om neg(Expression2) och Expression1 -> Expression2 g�ller
% neg(Expression1).
check_rule(_, [RowNr, neg(Expression1), mt(X, Y)], Proof) :-
    X < RowNr, Y < RowNr,
    get_row(X, Proof, imp(Expression1, Expression2), Rule1),
    not(Rule1 = box(_)),
    get_row(Y, Proof, neg(Expression2), Rule2),
    not(Rule2 = box(_)).
% Rad X ska vi anta neg(Expression) och rad Y en mots�gelse ger
% Expression.
check_rule(_, [RowNr, Expression, pbc(X, Y)], Proof) :-
    X < RowNr, Y < RowNr, X =< Y,
    get_row(X, Proof, neg(Expression), box(X)),
    get_row(Y, Proof, cont, box(X)).
% Kan f� Expression || neg(Expression) fr�n ingenting.
check_rule(_, [_, or(Expression, neg(Expression)), lem], _).
% Antagande utanf�r box�ppning b�r alltid vara falskt.
check_rule(_, [_, _, assumption], _) :- fail.
% �r Expression en faktiskt premiss?
check_rule(Prems, [_, Expression, premise], _) :-
    member(Expression, Prems).

% Basfall, vi �r i slutet av beviset. Vi validerar regeln och kollar
% sedan om Expression �r detsamma som m�let.
valid_proof(Prems, Goal, [[RowNr, Expression, Rule]], Proof) :-
    last(Proof, [RowNr, Expression, Rule]),
    !,
    check_rule(Prems, [RowNr, Expression, Rule], Proof),
    Expression = Goal.
% Vi �r i slutet av en box. Vi validerar regeln.
valid_proof(Prems, _, [[RowNr, Expression, Rule]], Proof) :-
    !,
    check_rule(Prems, [RowNr, Expression, Rule], Proof).
% En box �ppnas, f�rsta raden m�ste vara ett antagande.
% Vi g�r rekursivt igenom hela boxen
% Vi "tar sedan bort" boxen
% Vi g�r igenom resten av beviset
valid_proof(Prems, Goal, [[[RowNr, _, assumption] | Box] | FollowingProof], Proof) :-
    valid_proof(Prems, Goal, Box, Proof),
    remove_box(RowNr, Proof, NewProof),
    valid_proof(Prems, Goal, FollowingProof, NewProof).
% En box �ppnas, finns bara en rad i den.
% F�rsta raden �r en assumption och beh�ver inte valideras.
% Vi "tar bort" boxen och g�r rekursivt igenom resten av beviset.
valid_proof(Prems, Goal, [[[RowNr, _, assumption]] | FollowingProof], Proof) :-
    remove_box(RowNr, Proof, NewProof),
    valid_proof(Prems, Goal, FollowingProof, NewProof).
% Vi kollar att raden st�mmer och g�r sedan till n�sta rad.
valid_proof(Prems, Goal, [Row | FollowingProof], Proof) :-
    !,
    check_rule(Prems, Row, Proof),
    valid_proof(Prems, Goal, FollowingProof, Proof).

% remove_box(Radnummer, Bevis med box, Bevis utan box).
/*RowNr �r p� den f�rsta raden i boxen. Vi s�tter "Bevis utan box" att
 vara en lista som inneh�ller tv� rader: rad 1 i boxen men med regeln
 box(RowNr), och rad 2 men med regeln box(RowNr). Sedan s�tter vi
 kriteriumet att rad 2 �r den sista raden i "Bevis med box". */
remove_box(RowNr, [[RowNr, Expression, assumption] | Box], [[RowNr, Expression, box(RowNr)], [RowNr2, Expression2, box(RowNr)]]) :-
    last(Box, [RowNr2, Expression2, _]).
% "Bevis med box" inneh�ller enbart en rad, "Bevis utan box" ska d� vara
% denna rad men med regeln box(RowNr).
remove_box(RowNr, [[RowNr, Expression, assumption]], [[RowNr, Expression, box(RowNr)]]).
% Vi har funnit en inre box. Huvudet p� "Bevis med box" �r en box,
% huvudet p� "Bevis utan box" ska d� ocks� vara en box. Vi k�r
% remove_box rekursivt med dessa boxar.
remove_box(RowNr, [[Row | Box] | Proof], [[NewRow | NewBox] | Proof]) :-
    remove_box(RowNr, [Row | Box], [NewRow | NewBox]).
% Huvudet p� "Bevis med box" �r en rad eller box, huvudet p� "Bevis utan
% box" ska vara detsamma. Vi k�r remove_box rekursivt med de rader som
% kommer efter den nuvarande raden. (Om vi med denna metod r�kar hoppa
% �ver boxen som vi ska till och komma till slutet av beviset s� kommer
% metoden att fallera. D� kommer den backtracka och k�ra metoden �ver
% och g� in i boxen.)
remove_box(RowNr, [Row | Proof], [Row | NewProof]) :-
    remove_box(RowNr, Proof, NewProof).

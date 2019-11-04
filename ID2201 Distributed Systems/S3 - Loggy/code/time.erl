-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

zero() ->
	0.

inc(_, T) ->
	T + 1.

merge(Ti, Tj) ->
	max(Ti, Tj).

leq(Ti, Tj) ->
	Ti =< Tj.

clock(Nodes) ->
	lists:map(fun(Node) -> {Node, 0} end, Nodes).

update(Node, Time, Clock) ->
	lists:keyreplace(Node, 1, Clock, {Node, Time}).

safe(_, []) ->
	true;
safe(Time, [{_, Time1} | Clock]) ->
	case leq(Time, Time1) of
		true ->
			safe(Time, Clock);
		false ->
			false
	end.

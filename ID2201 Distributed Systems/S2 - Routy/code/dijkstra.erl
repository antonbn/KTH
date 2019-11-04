-module(dijkstra).
-export([table/2, route/2]).

entry(Node, Sorted) ->
	case lists:keyfind(Node, 1, Sorted) of
		{Node, N, _} ->
			N;
		false ->
			0
	end.

replace(Node, N, Gateway, []) ->
	[{Node, N, Gateway}];
replace(Node, N, Gateway, [Entry | Sorted]) ->
	case Entry of
		{Node, _, _} ->
			replace(Node, N, Gateway, Sorted);
		{_, N2, _} ->
			if
				N =< N2 ->
					[{Node, N, Gateway}, Entry | lists:keydelete(Node, 1, Sorted)];
				true -> 
					[Entry | replace(Node, N, Gateway, Sorted)]
			end
	end.

update(Node, N, Gateway, Sorted) ->
	PrevN = entry(Node, Sorted),
	if
		N < PrevN ->
			replace(Node, N, Gateway, Sorted);
		true ->
			Sorted
	end.

iterate(Sorted, Map, Table) ->
	case Sorted of
		[{_, inf, _} | _] ->
			Table;
		[] ->
			Table;
		[{Node, N, Gateway} | Tail] ->
			ReachableNodes = map:reachable(Node, Map),
			UpdatedSorted = lists:foldl(
							  fun(Reachable, Accumulator) -> 
									  update(Reachable, N + 1, Gateway, Accumulator)
							  end,
							  Tail, ReachableNodes),
			iterate(UpdatedSorted, Map, [{Node, Gateway} | Table])
	end.

table(Gateways, Map) ->
	AccumulatorFunction = fun(Node, N, Gateway, Accumulator) ->
								  [{Node, N, Gateway} | Accumulator]
						  end,
	Sorted0 = lists:foldl(
			   fun(Node, Accumulator) -> 
					   case lists:member(Node, Gateways) of
						   true ->
							   Accumulator;
						   false ->
							   AccumulatorFunction(Node, inf, unknown, Accumulator)
					   end
			   end, 
			   [], map:all_nodes(Map)),
	Sorted = lists:foldl(
				fun(Gateway, Accumulator) ->
						AccumulatorFunction(Gateway, 0, Gateway, Accumulator)
				end, 
				Sorted0, Gateways),
	iterate(Sorted, Map, []).

route(Node, Table) ->
	case lists:keyfind(Node, 1, Table) of
		{Node, Gateway} ->
			{ok, Gateway};
		false ->
			notfound
	end.

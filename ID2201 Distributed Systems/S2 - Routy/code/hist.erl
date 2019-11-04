-module(hist).
-export([new/1, update/3]).

new(Name) ->
	[{Name, inf}].

update(Node, N, History) ->
	case lists:keyfind(Node, 1, History) of
		{Node, N0} ->
			if 
				N =< N0 ->
					old;
				true ->
					{new, [{Node, N} | lists:keydelete(Node, 1, History)]}
			end;
		false ->
			{new, [{Node, N} | History]}
	end.

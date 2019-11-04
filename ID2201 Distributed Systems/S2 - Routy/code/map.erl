-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new() -> 
	[].

update(Node, Links, Map) ->
	UpdatedMap = lists:keydelete(Node, 1, Map),
	[{Node, Links} | UpdatedMap].
 
reachable(Node, Map) ->
	case lists:keyfind(Node, 1, Map) of
		{_, Links} ->
			Links;
		false ->
			[]
	end.

%% Can probably make prettier
all_nodes([]) ->
	[];
all_nodes([{Node, Links} | Map]) ->
	Nodes = all_nodes(Map),
	case lists:member(Node, Nodes) of
		true ->
			all_nodes(Links, Nodes);
		false ->
			[Node | all_nodes(Links, Nodes)]
	end.
all_nodes([], Nodes) ->
	Nodes;
all_nodes([Head | Tail], Nodes) ->
	case lists:member(Head, Nodes) of
		true ->
			all_nodes(Tail, Nodes);
		false ->
			[Head | all_nodes(Tail, Nodes)]
	end.

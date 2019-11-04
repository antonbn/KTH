-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

new() ->
	[].

add(Name, Ref, Pid, Intf) ->
	[{Name, Ref, Pid} | Intf].

remove(Name, Intf) ->
	lists:keydelete(Name, 1, Intf).

lookup(Name, Intf) ->
	case Intf of
		[{Name, _, Pid} | _] ->
			{ok, Pid};
		[_ | Tail] ->
			lookup(Name, Tail);
		[] ->
			notfound
	end.

ref(Name, Intf) ->
	case Intf of
		[{Name, Ref, _} | _] ->
			{ok, Ref};
		[_ | Tail] ->
			ref(Name, Tail);
		[] ->
			notfound
	end.

name(Ref, Intf) ->
	case Intf of
		[{Name, Ref, _} | _] ->
			{ok, Name};
		[_ | Tail] ->
			name(Ref, Tail);
		[] ->
			notfound
	end.

list([]) ->
	[];
list([{Name, _, _} | Intf]) ->
	[Name | list(Intf)].

broadcast(_, []) ->
	ok;
broadcast(Message, [{_, _, Pid} | Intf]) ->
	Pid ! Message,
	broadcast(Message, Intf).

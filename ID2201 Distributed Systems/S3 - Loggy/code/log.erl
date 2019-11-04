-module(log).
-export([start/1, stop/1]).

start(Nodes) ->
	spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
	Logger ! stop.

init(Nodes) ->
	loop(time:clock(Nodes), []).

loop(Clock, Queue) ->
	receive
		{log, From, Time, Msg} ->
			Clock1 = time:update(From, Time, Clock),
			Queue1 = update(From, Time, Msg, Queue),
			Queue2 = log(Clock1, Queue1),
			loop(Clock1, Queue2);
		stop ->
			log([], Queue)
	end.

update(From, Time, Msg, []) ->
	[{From, Time, Msg}];
update(From, Time, Msg, [Entry | Queue]) ->
	{_, Time1, _} = Entry,
	case time:leq(Time, Time1) of
		true ->
			[{From, Time, Msg}, Entry | Queue];
		false ->
			[Entry | update(From, Time, Msg, Queue)]
	end.

log(_, []) ->
	[];
log(Clock, [{From, Time, Msg} | Queue]) ->
	case time:safe(Time, Clock) of
		true ->
			io:format("log: ~w ~w ~p\n", [Time, From, Msg]),
			log(Clock, Queue);
		false ->
			[{From, Time, Msg} | Queue]
	end.

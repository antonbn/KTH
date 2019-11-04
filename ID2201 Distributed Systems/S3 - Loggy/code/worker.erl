-module(worker).
-export([start/4, stop/1, peers/2]).

start(Name, Logger, Sleep, Jitter) ->
	spawn_link(fun() -> init(Name, Logger, Sleep, Jitter) end).

stop(Worker) ->
	Worker ! stop.

init(Name, Log, Sleep, Jitter) ->
	receive
		{peers, Peers} ->
			loop(Name, Log, Peers, Sleep, Jitter, time:zero());
		stop ->
			ok
	end.

peers(Wrk, Peers) ->
	Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Lamport) ->
	Wait = rand:uniform(Sleep),
	receive
		{msg, Time, Msg} ->
			Time1 = time:inc(Name, time:merge(Lamport, Time)),
			Log ! {log, Name, Time1, {received, Msg}},
			loop(Name, Log, Peers, Sleep, Jitter, Time1);
		stop ->
			ok;
		Error ->
			Log ! {log, Name, time, {error, Error}}
	after Wait ->
		Selected = select(Peers),
		Time = time:inc(Name, Lamport),
		Message = {hello, rand:uniform(100)},
		Selected ! {msg, Time, Message},
		jitter(Jitter),
		Log ! {log, Name, Time, {sending, Message}},
		loop(Name, Log, Peers, Sleep, Jitter, Time)
	end.

select(Peers) ->
	lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) ->
	ok;
jitter(Jitter) ->
	timer:sleep(rand:uniform(Jitter)).

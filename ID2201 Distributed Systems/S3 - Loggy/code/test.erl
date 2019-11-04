-module(test).
-export([run/2]).

%% report on initial observations
run(Sleep, Jitter) ->
	Log = log:start([john, paul, ringo, george]),
	A = worker:start(john, Log, Sleep, Jitter),
	B = worker:start(paul, Log, Sleep, Jitter),
	C = worker:start(ringo, Log, Sleep, Jitter),
	D = worker:start(george, Log, Sleep, Jitter),
	worker:peers(A, [B, C, D]),
	worker:peers(B, [A, C, D]),
	worker:peers(C, [A, B, D]),
	worker:peers(D, [A, B, C]),
	timer:sleep(5000),
	log:stop(Log),
	worker:stop(A),
	worker:stop(B),
	worker:stop(C),
	worker:stop(D).

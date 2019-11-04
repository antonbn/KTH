-module(node2).
-export([start/1, start/2, test/0]).

-define(Stabilize, 100).
-define(Timeout, 10000).

test() ->
	Peer = test:start(node2),
	timer:sleep(250),
	
	Keys = test:keys(5),
	test:add(Keys, Peer),
	timer:sleep(250),
	
	test:check(Keys, Peer),
	Suc1 =  test:start(node2, Peer),
	timer:sleep(250),
	Suc2 =  test:start(node2, Peer),
	timer:sleep(250),
	Suc3 =  test:start(node2, Peer),
	timer:sleep(250),
	
	Peer ! probe,
	Peer ! status,
	Suc1 ! status,
	Suc2 ! status,
	Suc3 ! status,
	test:check(Keys, Peer).

start(Id) ->
    start(Id, nil).

start(Id, Peer) ->
    timer:start(),
    spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, storage:create()).

connect(Id, nil) ->
    {ok, {Id, self()}};
connect(_Id, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
		{Qref, Skey} ->
		    {ok, {Skey, Peer}}
    after ?Timeout ->
	    io:format("Time out: no response~n",[])
    end.

node(Id, Predecessor, Successor, Store) ->
    receive
		{key, Qref, Peer} ->
		    Peer ! {Qref, Id},
		    node(Id, Predecessor, Successor, Store);
		{notify, New} ->
		    {Pred, UpdatedStore} = notify(New, Id, Predecessor, Store),
		    node(Id, Pred, Successor, UpdatedStore);
		{request, Peer} ->
		    request(Peer, Predecessor),
		    node(Id, Predecessor, Successor, Store);
		{status, Pred} ->
		    Succ = stabilize(Pred, Id, Successor),
		    node(Id, Predecessor, Succ, Store);
		stabilize ->
		    stabilize(Successor),
		    node(Id, Predecessor, Successor, Store);
	
		{add, Key, Value, Qref, Client} ->
		    Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
		    node(Id, Predecessor, Successor, Added);
		{lookup, Key, Qref, Client} ->
		    lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
		    node(Id, Predecessor, Successor, Store);
		{handover, Elements} ->
		    Merged = storage:merge(Store, Elements),
		    node(Id, Predecessor, Successor, Merged);
	
		probe ->
		    create_probe(Id, Successor),
		    node(Id, Predecessor, Successor, Store);
		{probe, Id, Nodes, T} ->
		    remove_probe(T, Nodes),
		    node(Id, Predecessor, Successor, Store);
		{probe, Ref, Nodes, T} ->
		    forward_probe(Ref, T, Nodes, Id, Successor),
		    node(Id, Predecessor, Successor, Store);
	
		status ->
			io:format("id: ~w, pred: ~w, suc: ~w, store: ~w\n\n", [Id, Predecessor, Successor, Store]),
		    node(Id, Predecessor, Successor, Store);
		stop ->
		    ok
    end.

stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
		nil ->
		    Spid ! {notify, {Id, self()}},
		    Successor;
		{Id, _} ->
		    Successor;
		{Skey, _} ->
		    Spid ! {notify, {Id, self()}},
		    Successor;
		{Xkey, Xpid} ->
		    case key:between(Xkey, Id, Skey) of
				true ->
				    Xpid ! {request, self()},
				    Pred;
				false ->
				    Spid ! {notify, {Id, self()}},
				    Successor
		    end
    end.

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

stabilize({_, Spid}) ->
    Spid ! {request, self()}.

request(Peer, Predecessor) ->
    case Predecessor of
		nil ->
		    Peer ! {status, nil};
		{Pkey, Ppid} ->
		    Peer ! {status, {Pkey, Ppid}}
    end.

notify({Nkey, Npid}, Id, Predecessor, Store) ->
    case Predecessor of
		nil ->
		    Keep = handover(Id, Store, Nkey, Npid),
		    {{Nkey, Npid}, Keep};
		{Pkey, _} ->
		    case key:between(Nkey, Pkey, Id) of
				true ->
				    Keep = handover(Id, Store, Nkey, Npid),
				    {{Nkey, Npid}, Keep};
				false ->
				    {Predecessor, Store}
		    end
    end.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
		true ->
		    Client ! {Qref, ok},
		    storage:add(Key, Value, Store);
		false ->
		    Spid ! {add, Key, Value, Qref, Client},
		    Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
		true ->
		    Result = storage:lookup(Key, Store),
		    Client ! {Qref, Result};
		false ->
		    Spid ! {lookup, Key, Qref, Client}
    end.

handover(Id, Store, Nkey, Npid) ->
    {Keep, Leave} = storage:split(Nkey, Id, Store),
    Npid ! {handover, Leave},
    Keep.

create_probe(Id, Successor) ->
    {_, Pid} = Successor,
    Pid ! {probe, Id, [Id], erlang:now()}.

remove_probe(Time, Nodes) ->
    T = timer:now_diff(erlang:now(), Time),
    io:format("Probe time: ~w, list ~w\n", [T, Nodes]).

forward_probe(Ref, Time, Nodes, Id, Successor) ->
    {_, Pid} = Successor,
    Pid ! {probe, Ref, Nodes ++ [Id], Time}.

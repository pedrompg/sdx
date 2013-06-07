-module(node2).
-export([start/1, start/2, init/2, node/4, notify/4, request/2, stabilize/3]).

-define(Stabilize, 2000).
-define(Timeout, 5000).

start(MyKey) ->
	start(MyKey, nil).

start(MyKey, PeerPid) ->
	timer:start(),
	spawn(fun() -> init(MyKey, PeerPid) end).	

init(MyKey, PeerPid) ->
	Predecessor = nil,
	{ok, Successor} = connect(MyKey, PeerPid),
	schedule_stabilize(),
	node(MyKey, Predecessor, Successor, []).

connect(MyKey, nil) ->
	{ok, {MyKey , self()}};

connect(_, PeerPid) ->
	Qref = make_ref(),
	PeerPid ! {key, Qref, self()},
	receive
		{Qref, Skey} ->
			{ok, {Skey , PeerPid}}
	after ?Timeout ->
		io:format("Timeout: no response from ~w~n", [PeerPid])
	end.

node(MyKey, Predecessor, Successor, Store) ->
	receive
		{key, Qref, PeerPid} ->
			PeerPid ! {Qref, MyKey},
			node(MyKey, Predecessor, Successor, Store);
		{notify, New} ->
			{Pred, St} = notify(New, MyKey, Predecessor, Store),			
			node(MyKey, Pred, Successor, St);
		{request, Peer} ->
			request(Peer, Predecessor),
			node(MyKey, Predecessor, Successor, Store);
		{status, Pred} ->
			Succ = stabilize(Pred, MyKey, Successor),
			node(MyKey, Predecessor, Succ, Store);
		stabilize ->
			stabilize(Successor),
			node(MyKey, Predecessor, Successor, Store);
		{add, Key, Value, Qref, Client} ->
			Added = add(Key, Value, Qref, Client, MyKey, Predecessor, Successor, Store),
			node(MyKey, Predecessor, Successor, Added);
		{lookup, Key, Qref, Client} ->
			lookup(Key, Qref, Client, MyKey, Predecessor, Successor, Store),
			node(MyKey, Predecessor, Successor, Store);
		{handover, Elements} ->
			Merged = storage:merge(Store, Elements),
			node(MyKey, Predecessor, Successor, Merged);
		probe ->
			create_probe(MyKey, Store, Successor),
			node(MyKey, Predecessor, Successor, Store);
		{probe, MyKey, Nodes, T} ->
			remove_probe(MyKey, Store, Nodes, T),
			node(MyKey, Predecessor, Successor, Store);
		{probe, RefKey, Nodes, T} ->
			forward_probe(RefKey, [MyKey|Nodes], Store, T, Successor),
			node(MyKey, Predecessor, Successor, Store)	
	end.

add(Key, Value, Qref, Client, MyKey, {Pkey, _}, {_, Spid}, Store) ->
	case key:between(Key, Pkey, MyKey) of
		true ->
			Added = storage:add(Key, Value, Store),
			Client ! {Qref, ok},
			Added;
		false ->
			Spid ! {add, Key, Value, Qref, Client},
			Store
	end.

lookup(Key, Qref, Client, MyKey, {Pkey, _}, {_, Spid}, Store) ->
	case key:between(Key , Pkey , MyKey) of
		true ->
			Result = storage:lookup(Key, Store),
			Client ! {Qref, Result};
		false ->
			Spid ! {lookup, Key, Qref, Client}
	end.


notify({Nkey, Npid}, MyKey, Predecessor, Store) ->
	case Predecessor of
		nil ->
			Keep = handover(Store, MyKey, Nkey, Npid),
			{{Nkey, Npid}, Keep};
		{Pkey, _} ->
			case key:between(Nkey, Pkey, MyKey) of
				true ->
					Keep = handover(Store, MyKey, Nkey, Npid),
					{{Nkey, Npid}, Keep};
				false ->
					{Predecessor, Store}
			end
	end.

handover(Store, MyKey, Nkey, Npid) ->
	{Keep, Leave} = storage:split(MyKey, Nkey, Store),
	Npid ! {handover, Leave},
	Keep.

request(Peer, Predecessor) ->
	case Predecessor of
		nil ->
			Peer ! {status, nil};
		{Pkey, Ppid} ->
			Peer ! {status, {Pkey, Ppid}}
	end.

stabilize(Pred, MyKey, Successor) ->
	{Skey, Spid} = Successor,
	case Pred of
		nil ->
			Spid ! {notify, {MyKey, self()}},
			Successor;
		{MyKey, _} ->
			Successor;
		{Skey, _} ->
			Spid ! {notify, {MyKey, self()}},
			Successor;
		{Xkey, Xpid} ->
			case key:between(Xkey, MyKey, Skey) of
				true ->
					self() ! stabilize,
					{Xkey, Xpid};
				false ->
					Spid ! {notify, {MyKey, self()}},
					Successor
	end
end.

stabilize({_, Spid}) ->
	Spid ! {request, self()}.

create_probe(MyKey, Store, {_, Spid}) ->
	Spid ! {probe, MyKey, [MyKey], erlang:now()},
	io:format("Create probe ~w! Store ~w~n", [MyKey, Store]).

remove_probe(MyKey, Store, Nodes, T) ->
	Time = timer:now_diff(erlang:now(), T),
	io:format("Received probe ~w in ~w ms Ring: ~w Store ~w~n", [MyKey, Time, Nodes, Store]).

forward_probe(RefKey, Nodes, Store, T, {_, Spid}) ->
	Spid ! {probe, RefKey, Nodes, T},
	io:format("Forward probe ~w! Store ~w~n", [RefKey, Store]).

schedule_stabilize() ->
	timer:send_interval(?Stabilize, self(), stabilize).

-module(gms3).
-export([start/1, start/2]).

-define(arghh, 100).

start(Id) ->
	Rnd = random:uniform(1000),
	Self = self(),
	spawn_link(fun()-> init(Id, Rnd, Self) end).

init(Id, Rnd, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	leader(Id, Master, 0, []).

start(Id, Grp) 	->
	Rnd = random:uniform(1000),
	Self = self(),
	spawn_link(fun()-> init(Id, Rnd, Grp, Self) end).

init(Id,  Rnd, Grp, Master) ->
	random:seed(Rnd, Rnd, Rnd),
	Self = self(),
	Grp ! {join, Self},
	receive
		{view, N, State, Leader, Peers} ->
			Master ! {ok, State},
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N + 1, State, Peers)
	end.

leader(Id, Master, N, Peers) ->
	receive
		{mcast, Msg} ->
			io:format("mcast N ~w~n", [N]),
			bcast(Id, {msg, N, Msg}, Peers), 
			Master ! {deliver, Msg},	
			leader(Id, Master, N + 1, Peers);
		{join, Peer} ->
			io:format("join N ~w~n", [N]),
			Master ! request,
			joining(Id, N, Master, Peer, Peers); 
		stop ->
			ok;
		Error ->
			io:format("leader ~w: strange message ~w~n", [Id, Error])
	end.

joining(Id, N, Master, Peer, Peers) ->
	receive
		{ok, State} ->
			Peers2 = lists:append(Peers, [Peer]),
			bcast(Id, {view, N, State, self(), Peers2}, Peers2),
			leader(Id, Master, N + 1, Peers2);
		stop ->
			ok
	end.
	
slave(Id, Master, Leader, N, Last, Peers) ->
	receive
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, N, Last, Peers);
		{join, Peer} ->
			Leader ! {join, Peer},
			slave(Id, Master, Leader, N, Last, Peers);
		{msg, N, Msg} ->
			Master ! {deliver, Msg},
			io:format("msg - Id ~w  N ~w  Last ~w ~n", [Id, N, Last]),
			slave(Id, Master, Leader, N + 1, {msg, N, Msg}, Peers);
		{msg, I, _} when I < N ->
			io:format("repeated msg - Id ~w  N ~w  Last ~w ~n", [Id, N, Last]),
			slave(Id, Master, Leader, N, Last, Peers);
		{view, N, State, Leader, View} ->
			io:format("View - Id ~w  N ~w  Last ~w ~n", [Id, N, Last]),
			slave(Id, Master, Leader, N + 1, {view, N, State, Leader, View}, View);
		{view, I, _, _, _} when I < N ->
			io:format("repeated view - Id ~w  N ~w  Last ~w ~n", [Id, N, Last]),
			slave(Id, Master, Leader, N, Last, Peers);
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master, N, Last, Peers);
		stop ->
			ok;
		Error ->
			io:format("slave ~w: strange message N ~w  Error ~w~n", [Id, N, Error])
		after 1000 ->
			Master ! {error, "no reply from leader"}
	end.

 election(Id, Master, N, Last, Peers) ->
	[Leader|Rest] = Peers,
	if
		Leader == self() ->
			bcast(Id, Last, Rest),
			Master ! {deliver, Last},
			leader(Id, Master, N, Rest);	
		true ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N, Last, Rest)
	end.


bcast(Id, Msg, Nodes) ->
	lists:foreach(fun(Node) ->
					Node ! Msg,
					crash(Id)
				  end,
				  Nodes).
				  
crash(Id) ->
	case random:uniform(?arghh) of
		?arghh ->
			io:format("leader ~w: crash~n", [Id]),
			exit(no_luck);
		_ ->
			ok
	end.


 

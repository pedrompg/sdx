-module(gms1).
-export([start/1, start/2]).

start(Id) ->
	Self = self(),
	spawn_link(fun()-> init(Id, Self) end).

init(Id, Master) ->
	leader(Id, Master, []).

start(Id, Grp) 	->
	Self = self(),
	spawn_link(fun()-> init(Id, Grp, Self) end).

init(Id, Grp, Master) ->
	Self = self(),
	Grp ! {join, Self},
	receive
	{view, State, Leader, Peers} ->
		Master ! {ok, State},
		slave(Id, Master, Leader, Peers)
	end.

leader(Id, Master, Peers) ->
	receive
		{mcast, Msg} ->
			bcast(Id, ..., ...), %% TODO: COMPLETE
		%% TODO: ADD SOME CODE
			leader(Id, Master, Peers);
		{join, Peer} ->
		%% TODO: ADD SOME CODE
			joining(Id, ..., ..., ...); %% TODO: COMPLETE
		stop ->
			ok;
		Error ->
			io:format("leader ~w: strange message ~w~n", [Id, Error])
	end.

joining(Id, Master, Peer, Peers) ->
	receive
		{ok, State} ->
			Peers2 = lists:append(Peers, [Peer]),
			bcast(Id, {view, State, self(), Peers2}, Peers2),
			leader(Id, Master, Peers2);
		stop ->
			ok
	end.

bcast(_, Msg, Nodes) ->
	lists:foreach(fun(Node) -> Node ! Msg end, Nodes).
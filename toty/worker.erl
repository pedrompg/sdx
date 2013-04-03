-module(worker).
-export([start/6]).
-define(change, 20).
start(Id, Grp, Module, Rnd, Sleep, Jitter) ->		
	spawn(fun() -> init(Id, Grp, Module, Rnd, Sleep, Jitter) end).
	
init(Id, Grp, Module, Rnd, Sleep, Jitter) ->
	random:seed(Rnd, Rnd, Rnd),
	Gui = gui:start(Id),
	Cast = apply(Module, start, [Rnd, self(), Jitter]),
	Grp ! {join, self(), Cast},
	receive
		{state, Color, Peers} ->
			Cast ! {peers, Peers},
			Gui ! {color, Color},
			cast_change(Id, Cast, Sleep),
			worker(Id, Cast, Color, Gui, Sleep),
			Cast ! stop,
			Gui ! stop
	end.

worker(Id, Cast, Color, Gui, Sleep) ->
	receive
		{deliver, {From, N}} ->
			Color2 = change_color(N, Color),
			Gui ! {color, Color2},
			if
				From == Id ->
					cast_change(Id, Cast, Sleep);
				true ->
					ok
			end,
			worker(Id, Cast, Color2, Gui, Sleep);
		stop ->
			ok;
		Error ->
			io:format("strange message: ~w~n", [Error]),
			worker(Id, Cast, Color, Gui, Sleep)
	end.

change_color(N, {R,G,B}) ->
	{G, B, ((R+N) rem 256)}.

cast_change(Id, Cast, Sleep) ->
	Msg = {Id, random:uniform(?change)},
	timer:send_after(Sleep, Cast, {send, Msg}).
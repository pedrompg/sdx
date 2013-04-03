-module(toty).
-export([start/3, stop/0]).
start(Module, Sleep, Jitter) ->
	register(toty, spawn(fun() -> init(Module, Sleep, Jitter) end)).

stop() ->
	toty ! stop.

init(Module, Sleep, Jitter) ->
	Self = self(),
	worker:start("1", Self, Module, 1, Sleep, Jitter),
	worker:start("2", Self, Module, 2, Sleep, Jitter),
	worker:start("3", Self, Module, 3, Sleep, Jitter),
	worker:start("4", Self, Module, 4, Sleep, Jitter),
	collect(4, [], []).

collect(N, Workers, Peers) ->
	if
		N == 0 ->
			Color = {0,0,0},
			lists:foreach(
				fun(W) ->
					W ! {state, Color, Peers}
				end,
				Workers),
			run(Workers);
		true ->
			receive
		{join, W, P} ->
			collect(N-1, [W|Workers], [P|Peers])
		end
	end.	

run(Workers) ->
	receive
		stop ->
		lists:foreach(
			fun(W) ->
				W ! stop
			end,
			Workers)
	end.
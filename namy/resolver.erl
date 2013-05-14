-module(resolver).
-export([start/1, stop/0, init/1]).

start(Root) ->
	register(resolver, spawn(resolver, init, [Root])).

stop() ->
	resolver ! stop,
	unregister(resolver).

init(Root) ->
	Empty = cache:new(),
	Inf = time:inf(),
	Cache = cache:add([], Inf, {domain, Root}, Empty),
	resolver(Cache).

resolver(Cache) ->
	receive
		{request, From, Req}->
			io:format("Line 20 - Resolver: request from ~w to solve ~w~n", [From, Req]),
			{Reply, Updated} = resolve(Req, Cache),
			io:format("Line 22 - resolver From ~w Reply ~w~n",[From, Reply]),
			From ! {reply, Reply},
			resolver(Updated);
		status ->
			io:format("Resolver: cache content: ~w~n", [Cache]),
			resolver(Cache);
		stop ->
			io:format("Resolver: closing down~n", []),
			ok;
		Error ->
			io:format("Resolver: reception of strange message ~w~n", [Error]),
			resolver(Cache)
	end.

resolve(Name, Cache)->
	io:format("Resolve ~w: ", [Name]),
	case cache:lookup(Name, Cache) of
		unknown ->
			io:format("unknown ~n", []),
			recursive(Name, Cache);
		invalid ->
			io:format("invalid ~n", []),
			NewCache = cache:remove(Name, Cache),
			recursive(Name, NewCache);
		{ok, Reply} ->
			io:format("found ~w~n", [Reply]),
			{Reply, Cache}
	end.

recursive([Name|Domain], Cache) ->
	io:format("Recursive - Name ~w Domain ~w Cache ~w: ~n", [Name,Domain,Cache]),
	case resolve(Domain, Cache) of
		{unknown, Updated} ->
			io:format("unknown ~n", []),
			{unknown, Updated};
		{{domain, Srv}, Updated} ->
			Srv ! {request, self(), Name},
			io:format("Line 58 - Resolver: sent request to solve [~w] to ~w~n", [Name, Srv]),
			receive
				{reply, unknown, _} ->
					{unknown, Updated};
				{reply, Reply, TTL} ->
					io:format("Line 64 - Reply ~w TTL ~w~n",[Reply, TTL]),
					Expire = time:add(time:now(), TTL),
					NewCache = cache:add([Name|Domain], Expire, Reply, Updated),
					io:format("Line 67 - recursive end reply ~w cache ~w~n",[Reply, Cache]),
					{Reply, NewCache}
			end
	end.

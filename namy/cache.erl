-module(cache).
-export([lookup/2, new/0, add/4, remove/2]).

lookup(Name, Cache) ->
	case lists:keyfind(Name, 1, Cache) of
		{_, Expire, Id} ->
			io:format("Cache - Line 7: Name ~w Expire ~w Id ~w~n", [Name, Expire, Id]),
			case time:valid(Expire, time:now()) of 
				true ->
					{ok, Id};
				false ->
					invalid
			end;
		false ->
			unknown
	end.

new() ->
	[].

add(NameDomain, Expire, Reply, Cache) ->
 	{_,Id} = Reply,
 	T = {NameDomain, Expire, Id},
 	io:format("Cache - Line 22: NameDomain ~w Expire ~w Id ~w~n", [NameDomain, Expire, Id]),
	lists:keystore(NameDomain, 1, Cache, T).

remove(Name, Cache) ->
	lists:keydelete(Name, 1, Cache).
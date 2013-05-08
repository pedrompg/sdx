-module(entry).
-export([lookup/2, add/3, remove/2]).

lookup(Req, Entries) ->
	io:format("Entry lookup - Req ~w Entries ~w~n", [Req, Entries]),
	lists:keyfind(Req, 1, Entries).

add(Name, Entry, Entries) ->
	io:format("Entry add - Name ~w Entry ~w Entries ~w~n", [Name, Entry, Entries]),
	lists:keystore(Name, 1, Entries, {Name, Entry}).

remove(Name, Entries) ->
	lists:keydelete(Name, 1, Entries).
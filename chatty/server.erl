-module(server).
-export([start/0, process_requests/1]).

start() ->
	ServerPid = spawn(server, process_requests, [[]]),
	register(myserver, ServerPid).

process_requests(Clients) ->
receive
	{client_join_req, Name, From} ->
		NewClients = [From|Clients], %% TODO: COMPLETE - done
		broadcast(NewClients, {join, Name}),
		process_requests(NewClients); %% TODO: COMPLETE - done

	{client_leave_req, Name, From} ->
		NewClients = lists:delete(From, Clients), %% TODO: COMPLETE - done
		broadcast(NewClients, {leave, Name}), %% TODO: COMPLETE - done
		From ! exit,
		process_requests(NewClients); %% TODO: COMPLETE - done

	{send, Name, Text} ->
		broadcast(Clients, {message,Name,Text}), %% TODO: COMPLETE - done
		process_requests(Clients);

	disconnect ->
		unregister(myserver)
end.

%% Local Functions
broadcast(PeerList, Message) ->
	Fun = fun(Peer) -> Peer ! Message end,
	lists:map(Fun, PeerList).
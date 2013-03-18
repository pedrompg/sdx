-module(server2).
-export([start/0, start/1, init_server/0, init_server/1]).

%% API Functions
start() ->
	ServerPid = spawn(server2, init_server, []),
	register(myserver, ServerPid).

start(BootServer) ->
	ServerPid = spawn(server2, init_server, [BootServer]),
	register(myserver, ServerPid).

init_server() ->
	process_requests([], [self()]).

init_server(BootServer) ->
	BootServer ! {server_join_req, self()},
	process_requests([], []).

process_requests(Clients, Servers) ->
	receive
	%% Messages between client and server
	{client_join_req, Name, From} ->
		NewClients = [From|Clients], %% TODO: COMPLETE - done
		io:format("[CLIENTS] ~w~n", [NewClients]),
		broadcast(NewClients, {join, Name}), %% TODO: COMPLETE - done
		broadcast(Servers, {join, Name}), %% TODO: COMPLETE - done
		process_requests(NewClients, Servers); %% TODO: COMPLETE - done

	{client_leave_req, Name, From} ->
		NewClients = lists:delete(From, Clients), %% TODO: COMPLETE - done
		broadcast(NewClients, {leave, Name}), %% TODO: COMPLETE - done
		From ! exit,
		process_requests(NewClients, Servers); %% TODO: COMPLETE - done

	{send, Name, Text} ->
		broadcast(Servers, {message, Name, Text}),
		process_requests(Clients, Servers);

	%% Messages between servers
	disconnect ->
		NewServers = lists:delete(self(), Servers),
		broadcast(NewServers, {update_servers, NewServers}), %% TODO: COMPLETE - done
		unregister(myserver);

	{server_join_req, From} ->
		NewServers = [From|Servers], %% TODO: COMPLETE - done
		broadcast(NewServers, {update_servers, NewServers}), %% TODO: COMPLETE - done
		process_requests(Clients, NewServers);

	{update_servers, NewServers} ->
		io:format("[SERVER UPDATE] ~w~n", [NewServers]),
		process_requests(Clients, NewServers); %% TODO: COMPLETE - done

	RelayMessage -> %% Whatever other message is relayed to its clients
		broadcast(Clients, RelayMessage),
		process_requests(Clients, Servers)
	end.

%% Local Functions
broadcast(PeerList, Message) ->
	Fun = fun(Peer) -> Peer ! Message end,
	lists:map(Fun, PeerList).
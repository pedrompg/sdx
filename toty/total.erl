-module(total).
-export([start/3]).
start(Id, Master, Jitter) ->
	spawn(fun() -> init(Id, Master, Jitter) end).

init(Id, Master, Jitter) ->
	random:seed(Id, Id, Id),
	receive
		{peers, Nodes} ->
			server(Master, seq:new(Id), seq:new(Id), Nodes, [], [], Jitter)
	end.

server(Master, NextPrp, MaxAgr, Nodes, Cast, Queue, Jitter) ->
	receive
		{send, Msg} ->
			Ref = make_ref(),
			request(Ref, Msg, Nodes, Jitter),
			NewCast = cast(Ref, Nodes, Cast),
			server(Master, NextPrp, MaxAgr, Nodes, NewCast, Queue, Jitter);
		{request, From, Ref, Msg} ->
			From ! {proposal, Ref, NextPrp},
			NewQueue = insert(Ref, Msg,  NextPrp, Queue),
			NewNextPrp = seq:increment(seq:max(NextPrp, MaxAgr)),
			server(Master, NewNextPrp, MaxAgr, Nodes, Cast, NewQueue, Jitter);
		{proposal, Ref, Proposal} ->
			case proposal(Ref, Proposal, Cast) of
				{agreed, MaxSeq, NewCast} ->
					agree(Ref , MaxSeq, Nodes),
					server(Master, NextPrp, MaxSeq, Nodes, NewCast, Queue, Jitter);
				NewCast ->
					server(Master, NextPrp, MaxAgr, Nodes, NewCast, Queue, Jitter)
				end;		
		{agreed, Ref, Seq} ->
			Updated = update(Ref, Seq, Queue),
			{Agreed, NewQueue} = agreed(Updated),
			deliver(Master, Agreed),
			NewMaxAgr = seq:max(Seq, MaxAgr),
			server(Master, NextPrp, NewMaxAgr, Nodes, Cast, NewQueue, Jitter);
		stop ->
			ok
	end.	

%% Sending a request message to all nodes
request(Ref, Msg, Nodes, 0) ->
	Self = self(),
	lists:foreach(fun(Node) ->
			Node ! {request, Self, Ref, Msg}
		end,
		Nodes);

request(Ref, Msg, Nodes, Jitter) ->
	Self = self(),
	lists:foreach(fun(Node) ->
		timer:sleep(random:uniform(Jitter)),
			Node ! {request, Self, Ref, Msg}
		end,
		Nodes).

%% Sending an agreed message to all nodes
agree(Ref, Seq, Nodes)->
	lists:foreach(fun(Pid)->
			Pid ! {agreed, Ref, Seq}
		end,
		Nodes).

%% Delivering messages to the master
deliver(Master, Messages) ->
	lists:foreach(fun(Msg)->
			Master ! {deliver, Msg}
		end,
		Messages).

%% Adding a new entry to the set of casted messages
cast(Ref, Nodes, Cast) ->
	L = length(Nodes),
	[{Ref, L, seq:new()}|Cast].

%% Update the set of casted messages
proposal(Ref, Proposal, [{Ref, 1, Sofar}|Rest])->
	{agreed, seq:max(Proposal, Sofar), Rest};

proposal(Ref, Proposal, [{Ref, N, Sofar}|Rest])->
	[{Ref, N-1, seq:max(Proposal, Sofar)}|Rest];

proposal(Ref, Proposal, [Entry|Rest])->
	case proposal(Ref, Proposal, Rest) of
		{agreed, Agreed, Rst} ->
			{agreed, Agreed, [Entry|Rst]};
		Updated ->
			[Entry|Updated]
	end.

%% Remove all messages in the front of the queue that have been agreed
agreed([{_Ref, Msg, agrd, _Agr}|Queue]) ->
	{Agreed, Rest} = agreed(Queue),
	{[Msg|Agreed], Rest};

agreed(Queue) ->
	{[], Queue}.

%% Update the queue with an agreed sequence number
update(Ref, Agreed, [{Ref, Msg, propsd, _}|Rest])->
	queue(Ref, Msg, agrd, Agreed, Rest);

update(Ref, Agreed, [Entry|Rest])->
	[Entry|update(Ref, Agreed, Rest)].

%% Insert a new message into the queue
insert(Ref, Msg, Proposal, Queue) ->
	queue(Ref, Msg, propsd, Proposal, Queue).

%% Queue a new entry
queue(Ref, Msg, State, Proposal, []) ->
	[{Ref, Msg, State, Proposal}];

queue(Ref, Msg, State, Proposal, Queue) ->
	[Entry|Rest] = Queue,
	{_,_,_,Next} = Entry,
	case seq:lessthan(Proposal, Next) of
		true ->
			[{Ref, Msg, State, Proposal}|Queue];
		false ->
			[Entry|queue(Ref, Msg, State, Proposal, Rest)]
	end.



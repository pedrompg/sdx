-module(lock3).
-export([init/2]).

init(Id, Nodes) ->
    open(Id, Nodes, 0).

open(Id, Nodes, Clock) ->
	receive
        {take, Master} ->
            Refs = requests(Id, Nodes, Clock),
            wait(Id, Nodes, Master, Refs, [], Clock, Clock);
        {request, From,  Ref, _, _} ->
            From ! {ok, Ref, Clock},
            open(Id, Nodes, Clock);
        stop -> ok
    end.

requests(Id, Nodes, Clock) ->
    lists:map(
      fun(P) ->
        R = make_ref(),
        P ! {request, self(), R, Id, Clock},
        R
        end, Nodes).

wait(Id, Nodes, Master, [], Waiting, Clock, _) ->
    Master ! taken,
    held(Id, Nodes, Waiting, Clock);

wait(Id, Nodes, Master, Refs, Waiting, Clock, MyTimestamp) ->
	receive
        {request, From, Ref, IdOther, Timestamp} ->
			if
				Timestamp < MyTimestamp ->
					Newclock = Clock + 1,
					From ! {ok, Ref, Newclock},
					R = make_ref(),
					From ! {request, self(), R, Id, MyTimestamp},
					wait(Id, Nodes, Master, [R | Refs], Waiting, Newclock, MyTimestamp);
		
				Timestamp > MyTimestamp ->
					Newclock = Timestamp + 1,
					wait(Id, Nodes, Master, Refs, [{From, Ref}|Waiting], Newclock, MyTimestamp);
				
				Timestamp == MyTimestamp ->
					Newclock = Clock + 1,
					if
						IdOther < Id -> 
							From ! {ok, Ref, Newclock},
							R = make_ref(),
							From ! {request, self(), R, Id, Newclock, MyTimestamp},
							wait(Id, Nodes, Master, [R | Refs], Waiting, Newclock, MyTimestamp);
						true ->
							wait(Id, Nodes, Master, Refs, [{From, Ref}|Waiting], Newclock, MyTimestamp) 
					end
			end;
        {ok, Ref, Timestamp} ->
			Refs2 = lists:delete(Ref, Refs),
            wait(Id, Nodes, Master, Refs2, Waiting, max(Clock,Timestamp) + 1, MyTimestamp);
        release ->
			ok(Waiting, Clock),
            open(Id, Nodes, Clock)
    end.

ok(Waiting, Clock) ->
    lists:map(
      fun({F,R}) ->
        F ! {ok, R, Clock}
      end,
      Waiting).

held(Id, Nodes, Waiting, Clock) ->
    receive
        {request, From, Ref, _, Timestamp} ->
            held(Id, Nodes, [{From, Ref}|Waiting], max(Clock,Timestamp) + 1);
        release ->
            ok(Waiting, Clock),
            open(Id, Nodes, Clock)
    end.

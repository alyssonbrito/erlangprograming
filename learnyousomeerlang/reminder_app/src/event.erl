%% @reference Learn you some erlang for great good
%% Reminder app
%% Event
%% When the event is done, it sends a message to the listener:
%% {done, Id}

-module(event).
%-export().
-compile(export_all). % TODO: make an export

%% Main State
-record(state, {server, name="", toGo=0}).

%% Stats
start(EventName, DateTime) ->
    spawn(?MODULE, init, [ self(), EventName, DateTime ]).
start_link(EventName, DateTime) ->
    spawn_link(?MODULE, init, [ self(), EventName, DateTime ]).


%% Cancel an event, deleting it.
cancel(Pid) ->
    Ref = erlang:monitor(process,Pid),
    Pid ! {self(), Ref, cancel},
    receive
	{Ref, ok} ->
	    erlang:demonitor(Ref,[flush]),
	    ok;
	{'DOWN', Ref, process, Pid, _Reason} ->
	    ok
    end.


%% Main loop
%% Messages {Pid, Ref, Msg}
loop(State = #state{server=Server, toGo = [ T | Next ]}) ->
    receive
	{Server, Ref, cancel} -> %State#state.server is not a valid patter to match on !!
	    Server ! {Ref, ok}
    after T*1000 ->
	      %% see fun normalize below
	      if Next =:= [] ->
	            Server! {done, State#state.name};
		 Next =/= [] ->
		     loop(State#state{toGo = Next})
	      end
    end.

%% Because Erl is limited to about 49 days (49 days * 24 hours * 60 minutes * 60 seconds *1000 ms)
%% in millisecods, the following function is used
%% We have to accommodate the new format in the loop
normalize(N) ->
    Limit = 49 * 24 * 60 * 60,
    [N rem Limit | lists:duplicate(N div Limit, Limit)].


%% ---------------------------------------------
%% Internals

init(Server, EventName, DateTime) ->
    loop(#state{server = Server,
	       name = EventName,
	       toGo = time_to_go(DateTime)}).

time_to_go(TimeOut = { {_,_,_} , {_,_,_} }) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
            calendar:datetime_to_gregorian_seconds(Now),
    Secs = if ToGo > 0 -> ToGo;
              ToGo =< 0 -> 0
	   end,
    normalize(Secs).






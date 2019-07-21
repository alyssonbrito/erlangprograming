%% @reference Learn you some erlang for great good
%% Reminder app
%% Server Module

-module(evserv).
%-export().
-compile(export_all). % TODO: make an export

%% ----------------------------------------------------------------------
%% INTERFACE
%% ----------------------------------------------------------------------

% Start the server
start() ->
    register(?MODULE, Pid = spawn( ?MODULE, init, [] )),
    Pid.

start_link() ->
    register(?MODULE, Pid = spawn_link( ?MODULE, init, [] )),
    Pid.

% Regular shutdown the serve
terminate() ->
    ?MODULE ! shutdown.

% Adds a new client
% Sucess: {ok, Ref}
% Failure: {error, Reason}
subscribe(Pid) ->
    Ref = erlang:monitor(process, whereis(?MODULE)),
    ?MODULE ! { self(), Ref, {subscribe, Pid} },
    receive
	{Ref, ok} -> {ok, Ref};
	{'DOWN', Ref, process, _Pid, Reason} -> {error, Reason}
    after 5000 ->
	      {error, timeout}
    end.

% Add one event to the alarm system
add_event(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
	{Ref, Msg} -> Msg
    after 5000 ->
	{error, timeout}
    end.

% Add one event to the alarm system
% in case of error, this version crashed the client
add_event2(Name, Description, TimeOut) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {add, Name, Description, TimeOut}},
    receive
	{Ref, {error, Reason}} -> erlang:error(Reason);
	{Ref, Msg} -> Msg
    after 5000 ->
	{error, timeout}
    end.

% Cancel an event
cancel(Name) ->
    Ref = make_ref(),
    ?MODULE ! {self(), Ref, {cancel, Name}},
    receive
	{Ref, ok} -> ok
    after 5000 -> {error, timeout}
    end.

% Dont know what this is for yet ...
listen(Delay) ->
    receive
	M = {done, _Name, _Description} ->
	    [M | listen(0)]
    after Delay*1000 -> []
    end.

%% Main State
%%  - list of events
%%  - list of clients
-record(state, {events, clients}).
%% List of events
-record(event, {name = "",
	       description = "",
	       pid,
	       timeout = { {1970,1,1}, {0,0,0} }}).

init() ->
    % Loadind events from a static file could b done here.
    loop(#state{
	    events = orddict:new(),
	    clients = orddict:new()
	   }).

%% Main loop
%% State: 2 lists
%% 	- clients
%% 	- events
loop(State = #state{}) ->
    receive

	% We have a new subscription
        {Pid, MsgRef, {subscribe, ClientPid}} ->
	    Ref = erlang:monitor(process, ClientPid),
	    NewClients = orddict:store(Ref, ClientPid, State#state.clients),
	    Pid ! {MsgRef, ok},
	    loop(State#state{ clients = NewClients });

	% Add a new timer
        {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
	    case valid_datetime(TimeOut) of
		false ->
		    % we have got an incorret time
		    % report error
		    Pid ! {MsgRef, {error, bad_timeout}},
		    loop(State);
		true ->
		    % create the event, add to the list
		    % sends the ok to the client
		    EventPid = event:start_link(Name, TimeOut),
		    NewEvents = orddict:store(Name,
					      #event{name = Name,
						   description = Description,
						   pid = EventPid,
						   timeout = TimeOut},
					     State#state.events),
		    Pid ! {MsgRef, ok},
		    loop(State#state{events = NewEvents})
	    end;

	% Cancel that timer
	% Remove if from the list, if it is there
	% This functions never fails ...
        {Pid, MsgRef, {cancel, Name}} ->
	    Events = case orddict:find(Name, State#state.events) of
			 {ok, Event} ->
			     event:cancel(Event#event.pid),
			     orddict:erase(Name, State#state.events);
			 error ->
			     State#state.events
		     end,
	    Pid ! {MsgRef, ok},
	    loop(State#state{events = Events});

	% Timer was fired
	{done, Name} ->
	    case orddict:find(Name, State#state.events) of
		error ->
		    loop(State);
		{ok, Event} ->
		    send_to_clients({done, Event#event.name, Event#event.description},
				   State#state.clients),
		    NewEvents = orddict:erase(Name, State#state.events),
		    loop(State#state{ events = NewEvents })
	    end;

	% regular exit process
	shutdown -> exit(shutdown);

	% A client has died. Remove it from the list
	{'DOWN', Ref, progress, _Pid, _Reason} ->
	    loop(State#state{ clients = orddict:erase(Ref, State#state.clients) });

	% Hot upgrade
	code_change ->
	    ?MODULE:loop(State);

	% Why I am getting this message? I dont know about it
	Unknown ->
	    io:format("Unknown message message: ~p~n",[Unknown]),
	    loop(State)
    end.


%% Internal
send_to_clients(Msg, ClientDict) ->
    orddict:map( fun(_Ref, Pid) -> Pid ! Msg end, ClientDict ).

%% Utils: validate date and time
valid_datetime({Date, Time}) ->
    try
        calendar:valid_date(Date) andalso valid_time(Time)
    catch
	error:function_clause -> false
    end;
valid_datetime(_) -> false.

valid_time({H, M, S}) -> valid_time(H, M, S).
valid_time(H,M,S) when H >= 0, H < 24,
		       M >= 0, M < 60,
		       S >= 0, S < 60 -> true;
valid_time(_,_,_) -> false.






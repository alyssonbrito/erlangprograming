% Examples from Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Example 5.8. Event Manager
% NOTE: incomplete. Lots os warnings regard not used variables.

-module(event_manager).
-export([start/2, stop/1]).
-export([add_handler/3, delete_handler/2, get_data/2, send_event/2]).
-export([init/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% APIs

%% Starts a generic event manager with alias <Name>
%% List {Handler, Data}, where Data is passed to callback init method
start(Name, HandlerList) ->
    register(Name, spawn(event_manager, init, [HandlerList])),
    ok.

%% Terminal all the handlers and stop the event manager process.
%% Return: List {Handler, Data}, where Data is the return of callback terminate.
stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply
    end.

%% Add the handler, and send Data to init callback
add_handler(Name, Handler, InitData) ->
    call(Name, {add_handler, Handler, InitData}).

%% Remove the handler. Calls callback terminate and return it value.
%% {error, instance} if the Handler does not exist
delete_handler(Name, Handler) ->
    call(Name, {delete_handler, Handler}).

%% Return states variable of the Handler
%% {error, instance} if Handler does not exist
get_data(Name, Handler) ->
    call(Name, {get_data, Handler}).

%% Send the Event to all handler
send_event(Name, Event) ->
    call(Name, {send_event, Event}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
init(HandlerList) ->
    loop(initialize(HandlerList)).
initialize([]) -> [];
initialize([{Handler, InitData} | Rest]) ->
    [{Handler, Handler:init(InitData)}| initialize(Rest)].

terminate([]) -> [];
terminate([{Handler, Data} | Rest]) ->
    [{Handler, Handler:terminate(Data)} | terminate(Rest)].

call(Name, Msg) ->
    Name ! {request, self(), Msg},
    receive {reply, Reply} -> Reply end.

reply(To, Msg) ->
    To ! {reply, Msg}.

loop(State) ->
    receive
	{request, From, Msg} ->
	    {Reply, NewState} = handle_msg(Msg, State),
	    reply(From, Reply),
	    loop(NewState);
	{stop, From} ->
	    reply(From, terminate(State))
    end.

handle_msg({add_handler, Handler, InitData}, LoopData) ->
    {ok, [{Handler, Handler:init(InitData)} | LoopData ] };
handle_msg({delete_handler, Handler}, LoopData) ->
    case lists:keysearch(Handler, 1 , LoopData) of
        false -> { {error, instance} , LoopData };
	{value, {Handler, Data} } ->
	    Reply = {data, Handler:terminate(Data)},
	    NewLoopData = lists:keydelete(Handler, 1, LoopData),
	    {Reply, NewLoopData}
    end;
handle_msg({get_data, Handler}, LoopData) ->
    case lists:keysearch(Handler, 1 , LoopData) of
        false -> { {error, instance} , LoopData };
	{value, {Handler, Data} } -> { {data,Data} , LoopData}
    end;
handle_msg({send_event, Event}, LoopData) ->
    { ok, event(Event, LoopData) }.

event(_Event, []) -> []; % no one to send to
event(Event, [ {Handler, Data} | Rest  ]) ->
    % evoke the handler callback, recursive go to the other handlers
    [ {Handler, Handler:handle_event(Event, Data)} | event (Event, Rest) ].


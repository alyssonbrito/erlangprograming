-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    % register (name, pid) -- usually name is the same as module
    register(frequency, spawn(frequency, init, [])).

init() ->
    % a pair, list of free frequencies , list of pair allocated frequencie {FREQ,PID}
    Frequencies = {get_frequencies(), []},
    % main loop
    loop(Frequencies).

% Hardcoded frequencies
get_frequencies() -> [10,11,12,13,14,1155].

%% client functions -- can also be used by supervisors
stop() -> call(stop).
allocate() -> call(allocate).
deallocate(Freq) -> call({deallocate,Freq}).

%% We hide all message passing and the message
%% protocol in a funcion interface.

call(Message) ->
    frequency ! {request, self(), Message},
    receive
	{reply, Reply} -> Reply
    end.

%% The main loop

loop(Frequencies) -> % List, List
    receive
	{request, Pid, allocate} ->
	    io:format("Got a allocate request ~n"),
	    io:format("-> List of Frequencies: ~p~n",[Frequencies]),
	    {NewFrequencies, Reply} = allocate(Frequencies, Pid),
	    io:format("<- List of Frequencies: ~p~n",[Frequencies]),
	    reply(Pid, Reply),
	    loop(NewFrequencies);
	{request, Pid, {deallocate, Freq}} ->
	    {NewFrequencies, Reply} = deallocate(Frequencies, Freq),
	    reply(Pid, Reply),
	    loop(NewFrequencies);
	{request, Pid, stop} ->
	    reply(Pid, ok)
    end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

%% internal helper funcions
%% allocate the first free Freq and add the pair
%% {Frep, Pid } to allocted list
allocate({[], Allocated}, _Pid) ->
    io:format("Allocated: ~p~n",[Allocated]),
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq | Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    Is_there = lists:keyfind(Freq,1,Allocated),
    if
	Is_there == false ->
	    {{Free, Allocated}, {error}};
	true ->
	    NewAllocated = lists:keydelete(Freq,1,Allocated),
	    {{[Freq | Free], NewAllocated}, {ok}}
    end.
%deallocate({Free, Allocated}, Freq) ->


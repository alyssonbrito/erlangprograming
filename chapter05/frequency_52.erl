%% @author Alysson Brito
%% Exercise 5.2. The frequency
%% @reference Based on the frequency example

-module(frequency_52).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    % register (name, pid) -- usually name is the same as module
    register(frequency, spawn(frequency_52, init, [])).

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
	    {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
	    reply(Pid, Reply),
	    loop(NewFrequencies);
	{request, Pid, stop} ->
		{_, Allocated} = Frequencies,
		if length(Allocated) == 0 -> reply(Pid, ok);
			true -> reply(Pid,cannot_stop_now), loop(Frequencies)
		end
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
	% new requirement: up to free frequencies per client
	This_client_frequencies = [ TFreq || {TFreq, TPid} <- Allocated, Pid == TPid],
	if length(This_client_frequencies) < 3 ->
    	{{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}};
		true -> {{[Freq | Free], Allocated}, {error, max_freq_limit_reached}}
	end.

%% @TODO Use keytake in the future
deallocate({Free, Allocated}, Freq, Pid) ->
    case lists:keyfind(Freq,1,Allocated) of
		{Freq, Pid} ->
			io:format("deallocate: Got it~n"),
	    	NewAllocated = lists:keydelete(Freq,1,Allocated),
	    	{{[Freq | Free], NewAllocated}, {ok}};
		{Freq, _} ->
			io:format("deallocate: Just Freq~n"),
	    	{{Free, Allocated}, {error}};
		false ->
			io:format("deallocate: False~n"),
			% same as before, but we might want to add a different error
			% code in the future
	    	{{Free, Allocated}, {error}}
    end.


% f(). c(frequency_52).  f().
% frequency_52:start(). frequency_52:stop(). frequency_52:start().
% frequency_52:allocate().
% frequency_52:allocate().
% frequency_52:stop().
% frequency_52:deallocate(10).
% frequency_52:deallocate(10000).
% frequency_52:stop().
% % frequency_52:cdsacdscdcdsa(10000). -- kill the process
% % frequency_52:deallocate(11).
% frequency_52:allocate().
% frequency_52:allocate().
% frequency_52:allocate().
% frequency_52:allocate().
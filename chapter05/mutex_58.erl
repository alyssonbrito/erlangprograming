%% @doc Example 5.8 Mutex
%% @reference Examples from Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% NOTE: module name was changed!

-module(mutex_58).
-export([start/0,stop/0]).
-export([wait/0,signal/0]).
-export([init/0]).

%% Module management
start() ->
    register(mutex, spawn(?MODULE, init, [])).

stop() ->
    mutex ! stop.

%% Main API
wait() ->
    mutex ! {wait, self()},
    receive ok -> ok end.

signal() ->
    mutex ! {signal, self()}, ok.

%% Internal
init() -> free().

free() ->
    receive
	{wait, Pid} ->
	    Pid ! ok,
	    busy(Pid);
	stop ->
	    terminate()
    end.

busy(Pid) ->
    receive
	{signal, Pid} -> free()
    end.

terminate() ->
    receive
	% clean up the waiting list. Burn them all to heaven unconditionally!
	{wait, Pid} ->
	    exit(Pid, kill),
	    terminate()
    after
	0 -> ok
    end.


% c(mutex_58).
% mutex_58:start().
% mutex_58:wait().
% mutex_58:wait().
% mutex_58:signal().
% mutex_58:wait().
% mutex_58:signal().
% mutex_58:stop().


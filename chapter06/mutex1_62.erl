%% @doc Examples 5.8 from Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
%% @date 2019.jul 
%% @reference Exercise 6.2 based on the 
%% @author Alysson Brito (of changes required by the exercise)
%% @version 1/3: Adding link

-module(mutex_62).
-export([start/0,stop/0]).
-export([wait/0,superwait/0,signal/0]).
-export([init/0]).

%% Module management
start() ->
    register(mutex, spawn(?MODULE, init, [])).

stop() ->
    mutex ! stop.

%% Main API
wait() ->
    mutex ! {wait, self()},
    % testing ... kill myself before ack
    exit(byeByeWord),
    receive ok -> ok end.

superwait() ->
    mutex ! {wait, self()},
    % testing ... kill myself before ack
    receive ok -> ok end.

signal() ->
    mutex ! {signal, self()}, ok.

%% Internal
init() ->
    io:format("Traping exitgs... ~n"),
    process_flag(trap_exit, true), % this is the magic
    free().

free() ->
    receive
	{wait, Pid} ->
        io:format("Got a request... wait process time...~n"),
        sleep(8), % testing ... kill process here
        io:format("... going to link me to the client~n"),
        link(Pid), % before we give it the mutex
        io:format("... Link done. Going to send ack~n"),
	    Pid ! ok,
        io:format("... Moving to busy state ~n"),
	    busy(Pid);
	stop ->
	    terminate()
    end.

busy(Pid) ->
    receive
	    {signal, Pid} -> free();
        {'EXIT', Pid, Reason} -> % new case
                io:format("My client died on me. Pid: ~p  Reason:~p~n",[Pid, Reason]),
                free()
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

sleep(T) ->
    Timeout = T * 1000, % convert to miliseconds
    receive
    after Timeout -> true
    end.

% self().
% f(). c(mutex1_62). f().
% mutex1_62:start().
% mutex1_62:wait().
% mutex1_62:signal().


% mutex1_62:wait().
% mutex1_62:signal().
% mutex1_62:stop().

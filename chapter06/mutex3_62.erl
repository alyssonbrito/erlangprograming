%% @doc Examples 5.8 from Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
%% @date 2019.jul 
%% @reference Exercise 6.2 based on the 
%% @author Alysson Brito (of changes required by the exercise)
%% @version 3/3 with erlang:monitor

-module(mutex3_62).
-export([start/0,stop/0]).
-export([wait/0, superwait/0, signal/0]).
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

%% Because the regular wait has a kill on it, there is this
%% other wait that just request the resource
superwait() ->
    mutex ! {wait, self()},
    receive ok -> ok end.

signal() ->
    mutex ! {signal, self()}, ok.

%% Internal
init() ->
    io:format("Traping exitgs... ~n"),
    process_flag(trap_exit, true), % this is the magic
    free().

free() ->
    io:format("[FREE] I am~p ... Waiting requests...~n",[self()]),
    receive
	{wait, ClientPid} ->
        try 
            io:format("... Got a request from ~p... wait process time...~n",[ClientPid]),
            sleep(10), % testing ... kill process here
            io:format("... going to link me to the client~n"),
            % link(ClientPid), % before we give it the mutex
            erlang:monitor(process, ClientPid),
            io:format("... Link done. Going to send ack~n"),
	        ClientPid ! ok,
            io:format("... Moving to busy state ~n"),
	        busy(ClientPid)
        catch
            exit:Exit -> 
                io:format("... ERROR Could not link:~p~n",[Exit]),
                free();
            throw:Reason ->
                io:format("... TRHOW. ERROR Could not link:~p~n",[Reason]),
                free();
            _:_ ->
                io:format("... Got some exception. ERROR Could not link:~n"),
                free()
        end;
	stop ->
	    terminate()
    end.

busy(Pid) ->
    receive
	    {signal, ClientPid} ->
                unlink(ClientPid),
                free();
        {'DOWN', Reference, process, Pid, Reason} ->
                io:format("[BUSY] Monitor got a Down client soldier. Reference:~p Pid: ~p Reason:~p~n",[Reference, Pid, Reason]),
                free();
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

% self(). f(). c(mutex3_62). f(). mutex3_62:start(). self().
% mutex3_62:wait(). self().
% mutex3_62:signal(). -- get killed
% mutex3_62:superwait(). self(). -- wait to get the resource

% mutex3_62:wait().
% mutex3_62:signal().
% mutex3_62:stop().

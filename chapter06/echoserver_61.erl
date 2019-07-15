%% @author: alyssonbrito@gmail.com
%% @date 2019.Jul
%% @reference Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
%% @doc Exercise 6.1

-module(echoserver_61).
-export([start/0,print/1,stop/0]).
-export([loop/0]).

%% ------------- interface
start() ->
    process_flag(trap_exit, true), % this is the magic
    register(echoserver,spawn_link(echoserver_61,loop,[])).

print(Term) ->
    call({Term}).

%% suicide
stop() ->
    %call(stop).
    exit(whereis(echoserver), kill).

%% ------------- internal
call(Message) when Message == stop ->
    echoserver ! {self(), stop};
call(Message) ->
    echoserver ! {self(), print, Message}.

%% ------------- server main loop
loop() ->
    receive
    {'EXIT', Pid, Reason} ->
        io:format("Child was killed Pid:~p Reason:~p ~n",[Pid, Reason]);
	{_Pid, print, Msg} ->
	    io:format("~p~n",[Msg]),
	    loop();
	{_Pid, stop} ->
	    true
    end.

%% c(echoserver_61).
%% frequency_61:

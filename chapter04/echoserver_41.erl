% author: alyssonbrito@gmail.com
% 2019.Jul
% Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Exercise 4.1

-module(echoserver_41).
-export([start/0,print/1,stop/0]).
-export([loop/0]).

%% ------------- interface
start() ->
    register(echoserver_41,spawn(echoserver_41,loop,[])).

print(Term) ->
    call({Term}).

stop() ->
    call(stop).

%% ------------- internal
call(Message) when Message == stop ->
    echoserver_41 ! {self(), stop};
call(Message) ->
    echoserver_41 ! {self(), print, Message}.

%% ------------- server main loop
loop() ->
    receive
	{_Pid, print, Msg} ->
	    io:format("~p~n",[Msg]),
	    loop();
	{_Pid, stop} ->
	    true
    end.



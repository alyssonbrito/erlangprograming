%% @reference Examples from Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
%% Example 6.1. Process Links and exit signals
%% NOTE: chaged. value 100 will make the process die

-module(add_two_100).
-export([start/0, request/1, loop/0]).

start() ->
    process_flag(trap_exit, true), % this is the magic
    Pid = spawn_link(?MODULE, loop, []),
    register(add_two, Pid),
    {ok, Pid}.

request(Int) ->
    add_two ! {request, self(), Int},
    receive
	    {result, Result} -> Result;
	    {'EXIT', _Pid, Reason} -> {error, Reason} % new message
    after 1000 -> timeout
    end.

loop() ->
    io:format("Entering loop...~n"),
    receive
        {request, _Pid, 100} ->
            byeWorld;
	    {request, Pid, Msg} ->
	        Pid ! {result, Msg + 1},
            loop()
    end.
    


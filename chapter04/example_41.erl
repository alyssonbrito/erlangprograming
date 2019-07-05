% Examples from Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Example 4.1. The echo process
% NOTE: module name was changed!

-module(example_41).
-export([go/0, loop/0]).

go() ->
    Pid = spawn (example_41,loop,[]),
    Pid ! {self(), hello},
    receive
	{Pid, Msg} -> io:format("~w~n",[Msg])
    end,
    Pid ! stop.


loop() ->
    receive
	{From, Msg} ->
	    From ! {self(), Msg};
	stop ->
	    true
    end
    loop().



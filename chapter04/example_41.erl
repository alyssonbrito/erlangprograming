% Examples from Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Example 4.1. The echo process
% NOTE: module name was changed!

-module(example_41).
-export([go/0, loop/0]).

go() ->
    Yara = spawn (example_41,loop,[]),
    Yara ! {self(), maximiliano},
    receive
	{_, Msg} -> io:format("~w~n",[Msg])
    end,
    Yara ! {self(), hello},
    receive
	{_, Msg1} -> io:format("~w~n",[Msg1])
    end,
    Yara ! stop.

loop() ->
    receive
	{Remetente, Ms} ->
	    Remetente ! {self(), Ms},
	    loop();
	stop ->
	    true
    end.



-module(ask).
-export([term/0]).

term() ->
    Input = io:read("what {planem, distance} ? >>"),
    Term = element(2, Input),
    io:format("I Have: ~p~n",[Term]).

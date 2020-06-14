-module(count).
-export([countdown/1,countup/1]).

countdown(From) when From > 0 ->
    io:format("~w~n",[From]),
    countdown(From-1);
countdown(_) ->
    io:format("Blastoff!!~n").

countup(To) ->
    countup(0,To).

countup(Counter,To) when Counter < To ->
    io:format("~w~n",[Counter]),
    countup(Counter+1,To);
countup(_,_) ->
    io:format("BlastOff!~n").


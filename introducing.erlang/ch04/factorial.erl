-module(factorial).
-export([fact_nontail/1,fact_tail/1]).

fact_nontail(N) when N > 0 ->
    N * fact_nontail(N-1);
fact_nontail (0) ->
    1.

fact_tail(N) ->
    fact_acc(1, N).

fact_acc(Acc, N) when N > 0 ->
    io:format("Acc:~w N: ~w ~n",[Acc,N]),
    fact_acc(Acc * N, N-1);
fact_acc(Acc, 0) ->
    io:format("Bing Acc:~w ~n",[Acc]),
    Acc.



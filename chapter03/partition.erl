


-module(partition).
-export([partition_list/4]).

partition_list([],_A,_B,_I) -> [];
partition_list(_L,_A,B,I)  when I >= B ->
    io:format("¢¢¢¢¢¢¢¢¢¢¢¢¢¢ A:~p~n B:~p~n I:~p~n",[_A,B,I]),
    [];
partition_list([H|T],A,B,I) when true->
    io:format("###########   A:~p~n B:~p~n I:~p~n",[A,B,I]),
    partition_list(T,A,B,I+1);
partition_list([H|T],A,B,I) ->
    io:format("Entrei aqui com List: A:~p~n B:~p~n I:~p~n I<A:~p~n",[A,B,I,I<A]),
    [H] ++ partition_list(T,A,B,I+1).


%% @reference Learn You Some Erlang for great good
%% Funcionally solving problems
%% Heathrow to london example

-module(road).
-compile(export_all).

%% Read from file and make list
main([FileName]) ->
    {ok, Bin} = file:read_file(FileName),
    Map = optimal_path( parse_map(Bin) ),
    io:format("~p~n",[Map]).
    %erlang:halt().

%% Return a list of integer from binary (or string)
parse_map(Bin) when is_binary(Bin) ->
    parse_map(binary_to_list(Bin));
parse_map(Str) when is_list(Str) ->
    Values = [ list_to_integer(X) || X <- string:tokens(Str,"\r\n\t") ],
    group_vals(Values, []).

%% make tuples (A,B,X) from list of integers
group_vals([], Acc) ->
    lists:reverse(Acc);
group_vals([A, B, X | Rest], Acc) ->
    group_vals(Rest, [{A,B,X} | Acc]).

%% Pick the shortest to A1 and B1
%% Add to the Acc (distance, Path), in reverse order
shortest_step({A, B, X}, { {DistA, PathA} , {DistB, PathB} }) ->
    OptA1 = { DistA + A, [ {a, A} | PathA ] }, % to A -- coming straigh from A
    OptA2 = { DistB + B + X, [ {x, X}, {b, B} | PathB ] }, % to A -- comming from B, X
    OptB1 = { DistB + B, [ {b, B} | PathB ] }, % to B -- comming from B
    OptB2 = { DistA + A + X, [ {x, X}, {a, A} | PathA ] }, % -- to B, comming form A,X
    {erlang:min(OptA1, OptA2), erlang:min(OptB1, OptB2)}. % pick the minimum to A and to B

optimal_path(Map) ->
    {A, B} = lists:foldl(fun shortest_step/2, { {0,[]}, {0,[]} }, Map),
    {_Dist, Path} = if hd(element(2,A)) =/= {x,0} -> A; % get the last path is not X
		    hd(element(2, B)) =/= {x,0} -> B
		    end,
    io:format("A:~p~n",[A]),
    io:format("B:~p~n",[B]),
    lists:reverse(Path).


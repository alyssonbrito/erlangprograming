%% @doc Solution to Étude 6.4 List of Lists
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.1

-module(teeth064).
-export([alert/1, test/0]).


test() ->
    PocketDepths = [[0], [2,2,1,2,2,1], [3,1,2,3,2,3],
    [3,1,3,2,1,2], [3,2,3,2,2,1], [2,3,1,2,1,1],
    [3,1,3,2,3,2], [3,3,2,1,3,1], [4,3,3,2,3,3],
    [3,1,1,3,2,2], [4,3,4,3,2,3], [2,3,1,3,2,2],
    [1,2,1,1,3,2], [1,2,2,3,2,3], [1,3,2,1,3,3], [0],
    [3,2,3,1,1,2], [2,2,1,1,3,2], [2,1,1,1,1,2],
    [3,3,2,1,1,3], [3,1,3,2,3,2], [3,3,1,2,3,3],
    [1,2,2,3,3,3], [2,2,3,2,3,3], [2,2,2,4,3,4],
    [3,4,3,3,3,4], [1,1,2,3,1,2], [2,2,3,2,1,3],
    [3,4,2,4,4,3], [3,3,2,1,2,3], [2,2,2,2,3,3],
    [3,2,3,2,3,2]],
    alert(PocketDepths).

%% @doc Takes a list of 32 lists of six numbers as its input.
%% If a tooth isn’t present, it is represented by [0]
%% an return then day of the year
%% @param Teeths  List of list 32,6
%% A non existent tooth is represented as []
%% @return Number
-spec(alert(list()) -> number()).
alert(Teeths) ->
    lists:reverse(checkTooths(Teeths,1,[])).

checkTooths([],_, Acc) -> Acc;
checkTooths(Teeths,Counter, Acc) ->
    case cheeckTeeth(hd(Teeths)) of
        true -> checkTooths(tl(Teeths), Counter+1, [  Counter | Acc ] );
        false -> checkTooths(tl(Teeths), Counter+1, Acc )
    end.

cheeckTeeth([]) -> false;
cheeckTeeth(TeethPockets) when hd(TeethPockets) >= 4 -> true;
cheeckTeeth(TeethPockets)  -> cheeckTeeth(tl(TeethPockets)).



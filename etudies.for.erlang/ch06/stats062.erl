%% @doc Solution to Étude 6.2 Iteration through Lists (More Practice)
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.2

-module(stats062).
-export([minimum/1, maximum/1, range/1]).

%% @doc Recibes a list of numbers and returns the smalles
%% @param List of numbers
-spec(minimum(list()) -> number()).
minimum(ListOfNum) ->
    getMinimum(tl(ListOfNum),hd(ListOfNum)).

getMinimum([], Acc) -> Acc;
getMinimum(List, Acc) when hd(List) =< Acc -> getMinimum(tl(List),hd(List));
getMinimum(List, Acc) when hd(List) > Acc -> getMinimum(tl(List), Acc).

%% @doc Recibes a list of numbers and returns the smalles
%% @param List of numbers
-spec(maximum(list()) -> number()).
maximum(ListOfNum) ->
    getMaximum(tl(ListOfNum),hd(ListOfNum)).

getMaximum([], Acc) -> Acc;
getMaximum(List, Acc) when hd(List) < Acc -> getMaximum(tl(List),Acc);
getMaximum(List, Acc) when hd(List) >= Acc -> getMaximum(tl(List), hd(List)).

%% @doc Recibes a list of numbers and returns the [Min,Max]
%% @param List of numbers
-spec(range(list()) -> list()).
range(ListOfNum) ->
    [minimum(ListOfNum),maximum(ListOfNum)].


%% @doc Solution to Étude 6.1 Recursive Iteration through a List
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.1

-module(stats061).
-export([minimum/1]).

%% @doc Recibes a list of numbers and returns the smalles
%% @param List of numbers
-spec(minimum(list()) -> number()).

minimum(ListOfNum) ->
    getMinimum(tl(ListOfNum),hd(ListOfNum)).

getMinimum([], Acc) -> Acc;
getMinimum(List, Acc) when hd(List) =< Acc -> getMinimum(tl(List),hd(List));
getMinimum(List, Acc) when hd(List) > Acc -> getMinimum(tl(List), Acc).



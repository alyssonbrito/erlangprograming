%% @doc Solution to Étude 7.3: Using lists:foldl/3
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.2

-module(stats_073).
-export([mean/1, stdv/1]).
-export([minimum/1, maximum/1, range/1]).

%% @doc Calculates the mean of a list
mean(A) -> 
    % lists:foldl(fun(X, Sum) -> X + Sum end, 0, [1,2,3,4,5]). 15.
    sum(A) / length(A).

stdv(A) ->
    Sum = sum(A),
    SumOfSquares = sum_of_squares(A),
    N = length(A), 
    {_NSum_sq_, _Sum_sq} = sum_and_sum_of_square(A),
    io:format("Sum:[~p] SumOfQuares:[~p] Sum:[~p] SumOfSquare:[~p]~n",[Sum, SumOfSquares, _NSum_sq_, _Sum_sq]),
    R2 = (N * SumOfSquares - Sum * Sum) / (N * (N-1) ),
    io:format("R2:[~p]~n",[R2]),
    math:sqrt(R2).

-spec(sum_and_sum_of_square(list()) -> tuple()).
sum_and_sum_of_square(A) ->
    lists:foldl(fun(X, {H,T}) -> {H + X, T+ X*X} end, {0,0}, A).

-spec(sum(list()) -> number()).
sum(A) ->
    lists:foldl(fun(X, Sum) -> X + Sum end, 0, A).

-spec(sum_of_squares(list()) -> number()).
sum_of_squares(A) ->
    lists:foldl(fun(X, Sum) -> (X*X) + Sum end, 0, A).


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


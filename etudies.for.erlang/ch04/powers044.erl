%% @doc Solution to Étude 4.4 Tail Recursion with an Accumulator
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.2

-module(powers044).
-export([raise/2]).

%% @doc Calculates X to the power N
%% @param X base
%% @param N power
-spec(raise(number(),number()) -> number()).
raise(_X,0) -> 1;
raise(X,1) -> X;
raise(X,N) when N > 0 -> raise(X,N,1);
raise(X,N) when N < 0 -> 1/raise(X,-N,1).

%% @doc Calculates X to the power N
%% @param X base
%% @param N power
%% @param Acc Starting in 1
-spec(raise(number(),number(),number()) -> number()).
raise(_X,0, Acc) -> Acc;
raise(X,N, Acc) -> raise(X,N-1,X*Acc).

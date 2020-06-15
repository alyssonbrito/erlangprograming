%% @doc Solution to Étude 4.3 Non-Tail Recursive Functions
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.1

-module(powers043).
-export([raise/2]).

%% @doc Calculates X to the power N
%% @param X base
%% @param N power
-spec(raise(number(),number()) -> number()).
raise(_X,0) -> 1;
raise(X,1) -> X;
raise(X,N) when N > 0 -> X * raise(X,N-1);
raise(X,N) when N < 0 -> 1.0 / raise(X,-N).


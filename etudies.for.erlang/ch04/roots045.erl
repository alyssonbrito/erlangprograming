%% @doc Solution to Étude 4.5 Recursion with helper function
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.2

-module(roots045).
-export([raise/2, nth_root/2]).

%%% Newton-Raphson method for calculating roots
nth_root(X,N) ->
    nth_root(X, N, X / 2.0).

nth_root(X,N,A) ->
    io:format("Current guess is ~w~n",[A]),
    F = raise(A,N) - X,
    Fprime = N * raise(A,N-1),
    Next = A - F / Fprime,
    Change = abs(Next - A),
    % io:format("Change is ~w~n",[Change]),
    if
	Change < 1.0e-8 -> Next;
	true -> nth_root(X, N, Next)
    end.




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

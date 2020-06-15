%% @doc Solution to Étude 4.2 Recursion
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.1

-module(dijkstra042).
-export([gcd/2]).

%% @doc Calculates the greatest common divisor (GCD)
%% of two integer using Edsger W. Dijkstra
%% @param M
%% @param N
-spec(gcd(number(),number()) -> number()).
gcd(M,N) when M == N -> M;
gcd(M,N) when M > N -> gcd(M-N,N);
gcd(M,N) when M < N -> gcd(M,N-M).


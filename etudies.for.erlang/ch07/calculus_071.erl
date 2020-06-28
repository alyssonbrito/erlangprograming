%% @doc Solution to Étude 7.1 Simple Higher Order Functions
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.1

-module(calculus_071).
-export([derivative/2]).

%% @doc Calculate derivative by aproximation definition.
%% (Fn(X + H) - Fn(X)) / H
-spec(derivative(function(), float()) -> float()).
derivative(F,X) ->
    Delta = 1.0e-10,
    Res = (F(X+Delta) - F(X)) / Delta,
    Res.


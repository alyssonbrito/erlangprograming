% author: alyssonbrito@gmail.com
% 2019.Jun
% Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Exercise 2-2

-module(demo).
-export([double/1]).

% comment

double(Value) ->
    times(Value,2).
times(X,Y) ->
    X*Y.

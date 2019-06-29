% author: alyssonbrito@gmail.com
% 2019.Jun
% Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Exercise 3.1

-module(evaluating_31).
-export([sum/1]).

sum(0) -> 0;
sum(X) -> X + sum(X-1).


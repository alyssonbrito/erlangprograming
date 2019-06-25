% author: alyssonbrito@gmail.com
% 2019.Jun
% Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Exercise 2-3


-module(bool).
-export([b_not/1,b_and/2,b_or/2,b_nand/2]).

b_not(true) ->
    true;
b_not(false) ->
    false.
b_and(false,_) ->
    false;
b_and(_,false) ->
    false;
b_and(_,_) ->
    true.
b_or(true,_) ->
    true;
b_or(_, true) ->
    true;
b_or(_,_) ->
    false.
b_nand(V1,V2) ->
    b_or(b_not(V1),b_not(V2)).




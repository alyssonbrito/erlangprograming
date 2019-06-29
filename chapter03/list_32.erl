% author: alyssonbrito@gmail.com
% 2019.Jun
% Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Exercise 3.2

-module(list_32).
-export([create/1,reverse_create/1]).

create(1) -> [1];
create(N) ->
    create(N-1) ++ [N].

reverse_create(1) -> [1];
reverse_create(N) -> [N | reverse_create(N-1)].



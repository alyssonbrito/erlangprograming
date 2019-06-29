% author: alyssonbrito@gmail.com
% 2019.Jun
% Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Exercise 3.3

-module(side_effects_33).
-export([print_int/1,print_even/1]).

print_int(1) -> io:format("Number:~p~n",[1]);
print_int(N) ->
    print_int(N-1),
    io:format("Number:~p~n",[N]).

print_even(1) -> done;
print_even(N) when N rem 2 == 0 ->
    print_even(N-1),
    io:format("Number:~p~n",[N]);
print_even(N)  -> print_even(N-1).






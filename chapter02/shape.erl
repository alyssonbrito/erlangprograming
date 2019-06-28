% author: alyssonbrito@gmail.com
% 2019.Jun
% Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Exercise 2-2

-module(shape).
-export([area/1]).

% d

area({square,Side}) ->
    Side*Side;
area({circle, Radius}) ->
    math:pi()*Radius*Radius;
area({triangle, A, B, C}) ->
    S = (A+B+C)/2,
    math:sqrt(S*(S-A)*(S-B)*(S-C));
area(_Other) -> %also remove the _ of this variable and compile again to see the warning message
    {error, invalid_object}.


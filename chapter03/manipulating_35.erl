% author: alyssonbrito@gmail.com
% 2019.Jun
% Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Exercise 3.5


-module(manipulating_35).
-export([filter/2, reverse/1, concatenate/1, concatenate2/1, flatten/1]).


%% return all elements less or equal to N in a given list L
filter([], _N) -> [];
filter([ H | T ], N) when H =< N -> [ H | filter (T,N) ];
filter([ _H | T ], N) -> filter(T,N).


%% reverses a list L
reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].


%% Given a list of lists, concatenate them
%% version using concatenator operator
concatenate([]) -> [];
concatenate([H | T]) -> H ++ concatenate(T).


%% Given a list of lists, concatenate them
%% version without the concatenate operator
%%    and with a helper function
concatenate2([]) -> [];
concatenate2([H | T]) -> junta(H, T).

% Help function to concatenate lists
junta( [], []) -> [];
junta(L1, []) -> L1; %left side is always a simple list
junta([], [H | T]) -> junta(H,T); %right side is always a list of list, so we need to break it down
junta([ H1 | T1], L2 ) -> [H1 | junta(T1,L2)].


%% Return a flat list of list of nested lists
flatten([]) -> [];
flatten([H | T]) -> concatenate([flatten(H) , flatten(T)]);
flatten(K) -> [K].



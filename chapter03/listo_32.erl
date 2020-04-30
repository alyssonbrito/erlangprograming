% Exercise 3.2

-module(listo_32).
-export([create/1,reverse_create/1]).

%% N -> [1,2,3...,N]
create(N) ->
    create_acc(N,[]).

create_acc(0,Acc) -> Acc;
create_acc(N, Acc) ->
    create_acc((N-1), [N] ++ Acc).

% 5 [] -> (4, 5)
% 3 -> ()

reverse_create(1) -> [1];
reverse_create(N) -> [N | reverse_create(N-1)].

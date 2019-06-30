% author: alyssonbrito@gmail.com
% 2019.Jun
% Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Exercise 3.7

-module(db_37).
-export([new/0,destroy/1,write/3,delete/2,read/2,match/2]).

%% creates a new (empty) DB
new() -> [].


%% destroys a DB
destroy(_Db) -> ok.


%% add new element to existing DB.
write(Key,Element, Db) ->
    lists:append([{Key, Element}], Db).


%% delete element from existing DB.
delete(Key, Db) ->
    %lists:delete(Key,Db).
    %lists:filter(fun({K,V}) -> Key ==  K end, Db).
    lists:keydelete(Key,1,Db).


%% returns element from existing DB.
%read(_Key, []) -> {error, instance};
read(Key, Db) ->
    case lists:keyfind(Key, 1, Db) of
        {Key, Value} -> {ok,Value};
        false -> {error, instance}
    end.


%% returns list of all elements'key that have the given value in DB.
match (Value, Db) ->
    lists:filtermap(fun({K,V}) -> case V of Value -> {true, K}; _ -> false end end, Db).





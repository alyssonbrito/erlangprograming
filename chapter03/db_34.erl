% author: alyssonbrito@gmail.com
% 2019.Jun
% Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Exercise 3.4

-module(db_34).
-export([new/0,destroy/1,write/3,delete/2,read/2,match/2]).

%% creates a new (empty) DB
new() -> [].


%% destroys a DB
destroy(_Db) -> ok.


%% add new element to existing DB.
write(Key,Element, Db) ->
    [ {Key,Element} | Db ].


%% delete element from existing DB.
delete(_Key, []) -> [];
delete(Key, [ {Key, V} | T ]) -> delete(V,T) ;
delete(Key, [ H | T ]) -> [ H | delete (Key,T) ].


%% returns element from existing DB.
read(_Key, []) -> {error, instance};
read(Key, [ {Key, V} | _T ]) -> {Key,V};
read(Key, [ {_, _} | T ]) -> read (Key,T).


%% returns list of all elements'key that have the given value in DB.
match(_V, []) -> [];
match(V, [ {Key, V} | T ]) -> [ Key | match(V,T) ] ;
match(V, [ {_, _} | T ]) -> match (V,T).





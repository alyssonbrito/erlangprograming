%% @author: alyssonbrito@gmail.com
%% 2019.Jun
%% @Reference Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
%% Exercise 5.2

-module(db_51).
-export([write/2,delete/1,read/1,match/1]).
-export([start/0,stop/0]).
-export([loop/1]).

%% new interfaces
start() ->
    Db = db_new(),
    register(m_db, spawn(db_51,loop,[Db])),
    ok.

stop() ->
    call(stop),
    {Reply, Db} = get_reply(),
    db_destroy(Db),
    Reply.

write(Key,Element) ->
    call({write, {Key, Element}}),
    get_reply().
delete(Key) ->
    call({delete, Key}),
    get_reply().
read(Key) ->
    call({read, Key}),
    get_reply().
match(Element) ->
    call({match, Element}),
    get_reply().

% ------------------------------------
% Main loop
% ------------------------------------
loop(Db) -> 
    receive 
        {request, From, stop} -> 
            % bye bye cruel world! 
            reply({ok, Db}, From);
        {request, From, {write, Item} } ->
            {Reply, NewDb} = db_write(Db, Item),
            reply(Reply, From),
            loop(NewDb);
        {request, From, {delete, Key} } ->
            {Reply, NewDb} = db_delete(Db, Key),
            reply(Reply, From),
            loop(NewDb);
        {request, From, {read, Key} } ->
            Reply = db_read(Db, Key),
            reply(Reply, From),
            loop(Db);
        {request, From, {match, Element} } ->
            Reply = db_match(Db, Element),
            reply(Reply, From),
            loop(Db)
    end.

% ------------------------------------
% Utils
% ------------------------------------

% to main loop with protocol
call(Msg) ->
    m_db ! {request, self(), Msg}.

% answers back to client
reply(Msg, To) ->
    To ! Msg.

% wrap up message passing
get_reply() ->
    receive Reply -> 
        io:format("Got a reply:~p~n",[Reply]),
        Reply
    end.

% ------------------------------------
% DB management
% ------------------------------------

%% creates a new (empty) DB
db_new() -> [].

%% destroys a DB
db_destroy(_Db) -> ok.

%% add new element to existing DB.
db_write(Db,{Key, Element}) ->
    % [{Key, Element}] ++ DB
    NewDb = lists:append([{Key, Element}], Db),
    {ok, NewDb}.

%% delete element from existing DB.
db_delete(Db, Key) ->
    case lists:keyfind(Key,1,Db) of
        false -> {error, instance};
        {Key, Element} ->
            lists:keydelete(Key,1,Db),
            {ok, Element}
    end.

%% returns element from existing DB.
%read(_Key, []) -> {error, instance};
db_read(Db, Key) ->
    case lists:keyfind(Key, 1, Db) of
        {Key, Value} -> {ok,Value};
        false -> {error, instance}
    end.

%% returns list of all elements'key that have the given value in DB.
db_match (Db, Value) ->
    [Key || {Key, Element} <- Db, Value == Element].
    %lists:filtermap(fun({K,V}) -> case V of Value -> {true, K}; _ -> false end end, Db).

% f(). c(db_51). f().
% db_51:start().
% db_51:stop().
% db_51:start().
% db_51:write(foo, bar).
% db_51:read(foo).
% db_51:read(fooba).
% db_51:write(haruna, matata).
% db_51:write(pumba, matata).
% db_51:match(matata).
% db_51:match(barzar).
% db_51:stop().
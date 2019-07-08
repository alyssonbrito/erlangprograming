%% Examples from 
%% @reference Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
%% Example 6 Supervisor

-module(my_supervisor).
-export([start_link/2, stop/1]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
    register(Name, spawn_link(my_supervisor, init, [ChildSpecList])),
    ok.

stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.

%% Internal
init(ChildSpecList)->
    process_flag(trap_exit, true),
    loop(start_children(ChildSpecList)).

start_children([]) -> [];
start_children([ {M, F, A} | ChildSpecList ]) ->
    case (catch apply(M,F,A) )of
        {ok, Pid} ->
            [{Pid, {M,F,A}} | start_children(ChildSpecList)];
        _ ->
            start_children(ChildSpecList)
    end.

%% Main loop
restart_child(Pid, ChildList) ->
    io:format("List of Pid:~p Child:~p~n",[Pid, ChildList]),
    {value, {Pid, {M,F,A}} } = lists:keysearch(Pid,1, ChildList),
    io:format("Value:~p~n",[{M,F,A}]),
    {ok, NewPid} = apply(M,F,A),
    io:format("New List:~p~n",[lists:keydelete(Pid,1,ChildList)]),
    [ {NewPid, {M,F,A}} | lists:keydelete(Pid,1,ChildList) ].

loop(ChildList) ->
    receive
        {'EXIT', Pid, _Reason} ->
            NewChildList = restart_child(Pid, ChildList),
            loop(NewChildList);
        {stop, From} ->
            From ! {reply, terminate(ChildList)}
    end.

terminate([{Pid, _} | ChildList]) ->
    exit(Pid, kill),
    terminate(ChildList);
terminate(_ChildList) -> ok.

% f(). c(my_supervisor). c(add_two). f().
% my_supervisor:start_link(my_supervisor, [{add_two, start, []}]).
% exit(whereis(add_two),kill).
% add_two:request(100).
% whereis(add_two).

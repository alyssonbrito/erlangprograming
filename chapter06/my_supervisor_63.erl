%% Exercise 6.3 From
%% @reference Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
%% Based on Example 6 Supervisor
%% @date 2019.jul
%% Added type parameter

-module(my_supervisor_63).
-export([start_link/2, stop/1]).
-export([init/1]).

%% start the supervisor on this child
%% the child is added to the list inside the init function
%% @param Name: send to register
%% @param Module, Function, Argument: fun signature and arguments
%% @param Type: Either <permanent> or <transient>
%% <transient> means it is not restarted after normal exit
start_link(Name, [{Module, Function, Argument, Type}]) ->
    ChildPid = spawn_link(?MODULE, init, [[{Module, Function, Argument, Type}]]),
    register(Name, ChildPid),
    ok.

%% stops the supervisor, killing all children
stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.

%% ----------------------- Internal

%% trap the exit and add the children to the list
init(ChildSpecList)->
    process_flag(trap_exit, true),
    loop(start_children(ChildSpecList)).

start_children([]) -> [];
start_children([ {M, F, A, T} | ChildSpecList ]) ->
    case (catch apply(M,F,A) )of
        {ok, Pid} ->
            [ {Pid, T, {M,F,A}} | start_children(ChildSpecList) ];
        _ ->
            start_children(ChildSpecList)
    end.

%% Main loop
restart_child(Pid, ChildList) ->
    {value, {Pid, T, {M,F,A}} } = lists:keysearch(Pid,1, ChildList),
    NewList = lists:keydelete(Pid,1,ChildList),
    io:format("Temp List after delete:~p~n",[NewList]), % just
    io:format("Found? :~p~n",[{Pid, T, M,F,A}]),
    restart_child(Pid, NewList, 5).

restart_child(_Pid, _ChildList, 0) -> giveUp;
restart_child(Pid, ChildList, Tentatives) ->
    {value, {Pid, T, {M,F,A}} } = lists:keysearch(Pid,1, ChildList),
    try apply(M,F,A) of
        {ok, NewPid} ->
            io:format("Adding with new pid:~p~n",[NewPid]),
            [ {NewPid, T, {M,F,A}} | ChildList ]
    catch    
            _:_ -> 
            io:format("Error... trying again in 3s pid:~p tentatives:~p~n",[NewPid,Tentatives]),
            sleep(3),
            restart_child(Pid, ChildList, Tentatives-1)
    end.

loop(ChildList) ->
    receive
        {'EXIT', Pid, Reason} ->
            ProcessType = getProcessType(Pid,ChildList),
            if (Reason /= normal) orelse (ProcessType == permanent)  ->
                    io:format("Rise again my child: ~p Reason:~p~n",[Pid, Reason]),
                    NewChildList = restart_child(Pid, ChildList),
                    loop(NewChildList);
                true ->
                    %whatIsDeadMayNeverDay
                    io:format("bury my child: ~p Reason:~p~n",[Pid, Reason]),
                    NewChildList = removeProcess(Pid,ChildList),
                    loop(NewChildList)
            end;
        {stop, From} ->
            From ! {reply, terminate(ChildList)}
    end.

getProcessType(Pid,ChildList) ->
    Process = lists:keyfind(Pid,1,ChildList),
    % assuming it is always there...
    {_Pid, Type, {_M,_F,_A} } = Process,
    Type.

removeProcess(Pid, ChildList) ->
    lists:keydelete(Pid,1,ChildList).

terminate([{Pid, _} | ChildList]) ->
    exit(Pid, kill),
    terminate(ChildList);
terminate(_ChildList) -> ok.

sleep(T) ->
    Timeout = T * 1000, % convert to miliseconds
    receive
    after Timeout -> true
    end.

% f(). c(my_supervisor_63). c(add_two). f(). self().
% my_supervisor_63:start_link(my_supervisor_63, [{add_two, start, [], permanent}]).
% exit(whereis(add_two),kill).
% add_two:request(100).
% whereis(add_two).

% f(). c(my_supervisor_63). c(add_two_100). f(). self().
% my_supervisor_63:start_link(my_supervisor_63, [{add_two_100, start, [], transient}]).
% exit(whereis(add_two),kill).
% add_two:request(100).
% whereis(add_two).
% exit(whereis(add_two),normal).
% whereis(add_two).
% add_two:request(100).


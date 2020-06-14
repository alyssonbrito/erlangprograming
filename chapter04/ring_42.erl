% author: alyssonbrito@gmail.com
% 2019.Jul
% Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Exercise 4.2

-module(ring_42).
-export([start/3]).
-export([init/0]).

%% ---------------- interface
start(M, N, Message) ->
    Process_List = create_N_process(N),
    io:format("Process List:~p~n",[ Process_List]),
    connect_in_ring(Process_List),
    io:format("Sending messages...~n"),
    send_M_messages(Process_List,Message,M),
    send_quit(Process_List).

create_N_process(0) -> [];
create_N_process(N) ->
    Pid = spawn(ring_42,init,[]),
    io:format("Creating process Pid:[~p]~n",[ Pid ]),
    [Pid | create_N_process(N-1)].


init() ->
    loop(null).

connect_in_ring(ProcessList) when length(ProcessList) =< 1-> ProcessList;
    set_next(H,hd(T)),

connect_in_ring(ProcessList) ->
    io:format("Calling connect in ring [~p]~n",[ProcessList ++ [hd(ProcessList)]]),
    connect_in_ring_(ProcessList ++ [hd(ProcessList)]).
    %setHead(hd(ProcessList)]).

connect_in_ring_(ProcessListH) when length(ProcessListH) =< 1 -> ProcessListH;
connect_in_ring_([H| T]) ->
    io:format("Setting next:H[~p] T:[~p] TT:[~p]~n",[H,hd(T),T]),
    set_next(H,hd(T)),
    connect_in_ring_(T);
connect_in_ring_(P) ->
    io:format("ERRADO process list:[~p] ~n",[P]),
    io:format("ERRADO Head list:[~p] ~n",[list:hd(P)]),
    io:format("ERRADO Tail list:[~p] ~n",[list:tl(P)]).



%head, list of process
set_next(A,B) ->
    A ! {set_next, B}.

setHead(Pid) ->
    Pid ! {set_head, Pid}.

% kill them all!
send_quit([]) -> done;
send_quit([H | T]) ->
    io:format("Sending QUIT to H:[~p]~n",[ H ]),
    H ! {quit},
    send_quit(T).

% process A (next,)
loop(Next) ->
    receive
	{set_next,ProcessPid} ->
	    loop(ProcessPid);
	%{set_head,ProcessPid} ->
	%    loop(ProcessPid);
	{send_message, Pid, Msg, 0 } ->
	    Pid ! {ack, self()},
	{send_message, Pid, Msg, MN } ->
	    io:format("Self:[~p] Msg:[~p]~n",[ self() ,Msg ]),
	    if
		Next /= null ->
		    Next ! {send_message, self(), Msg, MN-1},
		    io:format("Inside the if ~n");
		Next == null -> nothing
	    end,
	    io:format("Looping"),
	    loop(Next);
	{quit} -> done;
	{ack, _Pid} -> done
    end.

send_M_messages(_ProcessList, _Message, 0) -> dona;
send_M_messages(ProcessList, Message, M) ->
    io:format("Sending M[~p] message to:[~p]~n",[M, hd(ProcessList)]),
    hd(ProcessList) ! {send_message, self(), Message},
    send_M_messages(ProcessList, Message, M-1),
    receive
	{ack, _Pid} -> quit
    end.







% author: alyssonbrito@gmail.com
% 2019.Jul
% Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Exercise 4.2

-module(ringo_42).
-export([start/3]).
-export([init/0]).

%% ---------------- interface
start(M, N, Message) ->
    Process_List = create_N_process(N),
    connect_in_ring(Process_List),
    send_message_to_ring(Process_List,Message,M*N),
    send_quit(Process_List).

% creates N process and return Pids as a list
create_N_process(0) -> [];
create_N_process(N) ->
    Pid = spawn(ringo_42,init,[]),
    [Pid | create_N_process(N-1)].

%% starts off the process
init() ->
    loop(null).

%% connects process as a circular list, link all to the next
%% and the latest to the head
connect_in_ring(ProcessList) ->
    io:format("Calling connect in ring [~p]~n",[ProcessList ++ [hd(ProcessList)]]),
    connect_in_ring_(ProcessList ++ [hd(ProcessList)]).

%helper function
connect_in_ring_( [H | [] ]) -> [H];
connect_in_ring_([ H | T]) ->
    %io:format("Setting next:H[~p] T:[~p] TT:[~p]~n",[H,hd(T),T]),
    set_next(H,hd(T)),
    connect_in_ring_(T).


% inform each process who is the next one
set_next(A,B) ->
    A ! {set_next, B}.

% burtn them all!
send_quit([]) -> done;
send_quit([H | T]) ->
    io:format("Sending QUIT to H:[~p]~n",[ H ]),
    H ! {quit},
    send_quit(T).

% main loop A
loop(Next) ->
    receive
	{set_next, ProcessPid} ->
	    loop(ProcessPid);
	{send_message, Sender, _Msg, 0 } ->
	    Sender ! {ack, self()},
	    loop(Next);
	{send_message, Sender, Msg, MN } ->
	    io:format("Self:[~p] Msg:[~p] MN[~p] ~n",[ self() ,Msg , MN]),
	    Next ! {send_message, Sender, Msg, MN-1},
	    loop(Next);
	{quit} -> done
    end.

send_message_to_ring(ProcessList, Message, M) ->
    hd(ProcessList) ! {send_message, self(), Message, M},
    receive
	{ack, _Pid} -> quit
    end.


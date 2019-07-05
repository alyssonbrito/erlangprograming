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
    %io:format("Process List:~p~n",[ Process_List]),
    connect_in_ring(Process_List),
    %io:format("Sending messages...~n"),
    send_message_to_ring(Process_List,Message,M*N),
    send_quit(Process_List).

create_N_process(0) -> [];
create_N_process(N) ->
    Pid = spawn(ringo_42,init,[]),
    %io:format("Creating process Pid:[~p]~n",[ Pid ]),
    [Pid | create_N_process(N-1)].


init() ->
    loop(null).

connect_in_ring(ProcessList) ->
    io:format("Calling connect in ring [~p]~n",[ProcessList ++ [hd(ProcessList)]]),
    connect_in_ring_(ProcessList ++ [hd(ProcessList)]).

connect_in_ring_( [H | [] ]) -> [H];
connect_in_ring_([ H | T]) ->
    %io:format("Setting next:H[~p] T:[~p] TT:[~p]~n",[H,hd(T),T]),
    set_next(H,hd(T)),
    connect_in_ring_(T).


%head, list of process
set_next(A,B) ->
    A ! {set_next, B}.

% kill them all!
send_quit([]) -> done;
send_quit([H | T]) ->
    io:format("Sending QUIT to H:[~p]~n",[ H ]),
    H ! {quit},
    send_quit(T).

% process A (next,)
loop(Next) ->
    receive
	{set_next, ProcessPid} ->
	    loop(ProcessPid);
	{send_message, Sender, _Msg, 0 } ->
	    Sender ! {ack, self()},
	    loop(Next);
	{send_message, Sender, Msg, MN } ->
	    io:format("-------------------------------------------------------~n"),
	    io:format("Self:[~p] Msg:[~p]~n",[ self() ,Msg ]),
	    io:format("Self:[~p] Msg:[~p]~n",[ self() ,Msg ]),
	    io:format("Self:[~p] Msg:[~p]~n",[ self() ,Msg ]),
	    io:format("Self:[~p] Msg:[~p]~n",[ self() ,Msg ]),
	    Next ! {send_message, Sender, Msg, MN-1},
	    io:format("Looping~n"),
	    loop(Next);
	{quit} -> done;
	{ack, _Pid} -> done
    end.

send_message_to_ring(ProcessList, Message, M) ->
   % io:format("Sending M[~p] message to:[~p]~n",[M, hd(ProcessList)]),
    hd(ProcessList) ! {send_message, self(), Message, M},
    %send_M_messages(ProcessList, Message, M-1),
    receive
	{ack, _Pid} -> quit
    end.






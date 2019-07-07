% Examples from Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Example 5.5. The FSM
% NOTE: module name define !

%% States: Idle (initial) , Ringin, Dial, Connect
%% Events: incoming , off_hook, on_hook, other_on_hook

-module(telephone_56).
-export([start/1, incoming/1, on_hook/0, off_hook/0, other_on_hook/1, dialNumber/1]).
-export([init/1]).

%% start up -> goes to idle
start(MyOwnNumber) ->
    Pid = spawn(telephone_56, init, [MyOwnNumber]),
    register(telephone_56, Pid), ok.

% internal helper
init(MyOwnNumber) ->
    idle(MyOwnNumber).

on_hook() ->
    call(on_hook).
off_hook() ->
    call(off_hook).
other_on_hook(Number) ->
    call({Number,other_on_hook}).
dialNumber(Number) ->
    call({Number, dial_number}).
incoming(Number) ->
    call({Number, incoming}).

call(Msg) ->
    telephone_56 ! Msg.

reply_to({Number, Msg}) ->
    io:format("============================~n"),
    io:format("Send to ~p : [~p]~n",[Number, Msg]).
    %Number ! Msg.


%% off_hook -> dial
%% incoming -> ringing
idle(MyOwnNumber) ->
    io:format("Idle [~p]...~n",[MyOwnNumber]),
    receive
	{Number, incoming} ->
	    start_ringing(Number),
	    ringing({MyOwnNumber, Number});
	off_hook ->
	    star_tone(),
	    dial(MyOwnNumber);
	A ->
	    io:format("IDLE Strange things: ~p~n",[A])
    end.

%% off_hook ==> connected
%% other_on_hook ==> idle
ringing({MyOwnNumber, Number}) ->
    io:format("Ringing [~p] From ~p...~n",[MyOwnNumber,Number]),
    receive
	{NewNumber, incoming} ->
	    io:format("Busy ringing [~p] From ~p...~n",[MyOwnNumber,NewNumber]),
	    reply_to({NewNumber, busy}),
	    ringing({MyOwnNumber,Number});
	{_Number, other_on_hook} ->
	    stop_ringing(),
	    idle(MyOwnNumber);
	off_hook ->
	    stop_ringing(),
	    connected({MyOwnNumber, Number})
    end.

%% on_hook ==> idle
connected({MyOwnNumber, Number}) ->
    io:format("Connected [~p] With ~p...~n",[MyOwnNumber,Number]),
    receive
	{NewNumber, incoming} ->
	    reply_to({NewNumber, busy}),
	    connected({MyOwnNumber,Number});
	on_hook ->
	    idle(MyOwnNumber)
    end.

%% off_hook ==> connected
%% other_on_hook ==> idle
dial(MyOwnNumber) ->
    io:format("Dialing  [~p] ...~n",[MyOwnNumber]),
    receive
	{NewNumber, incoming} ->
	    reply_to({NewNumber, busy}),
	    dial(MyOwnNumber);
	{_Number, on_hook} ->
	    stop_tone(),
	    idle(MyOwnNumber);
	{Number, dial_number} ->
	    stop_tone(),
	    call_number({MyOwnNumber, Number})
    end.

%% on_hook ==> idle
%% other_off_rook ==> connected
call_number({MyOwnNumber, Number}) ->
    io:format("Calling from  [~p] to ~p...~n",[MyOwnNumber,Number]),
    show_error(not_connected),
    idle(MyOwnNumber).


%% tephone external info
start_ringing(Number) ->
    io:format("START Ringing [~p]~n",[Number]).
stop_ringing() ->
    io:format("STOP Ringing ~n").
star_tone() ->
    io:format("START Tone~n").
stop_tone() ->
    io:format("STOP Tone~n").
show_error(Error) ->
    io:format("ERROR: ~p ~n",[Error]).











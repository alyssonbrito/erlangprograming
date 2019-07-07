% Examples from Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Example 5.2. The serve skeleton
% NOTE: module name was changed!

-module(server_skeleton).
-export([start/2, stop/1, call/2]).
-export([init/1]).

start(Name, Data) ->
    Pid = spawn(generic_handler, init, [Data]),
    register(Name,Pid),ok.

stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.

call(Name, Msg) ->
    Name ! {request, self(), Msg},
    receive {reply, Reply} -> Reply end.

reply(To, Msg) ->
    To ! {reply, Msg}.

init(Data) ->
    loop(initialize(Data)).

loop(State) ->
    receive
	{request, From, Msg} ->
	    {Reply, NewState} = hangle_msg(Msg, State),
	    reply(From, Reply),
	    loop(NewState);
	{stop, From} ->
	    reply(From, terminate(State))
    end.

initialize(...) -> ... .
hangle_msg(..., ...) -> ... .
terminate(...) -> ... .



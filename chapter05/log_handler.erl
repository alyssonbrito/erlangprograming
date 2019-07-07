% Examples from Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Example 5.9. log_handler : Example of handler that uses the event_manager

-module(log_handler).
-export([init/1, terminate/1, handle_event/2]).

% Need on all event handlers
init(File) ->
    {ok, Fd} = file:open(File,write),
    Fd.

% Need on all event handlers
% called on delete_handler/2 and stop/1
terminate(Fd) -> file:close(Fd).

%% Main handler
handle_event( { Action, Id, Event }, Fd  ) ->
    {MegaSec, Sec, MicroSec} = now(),
    Args = io:format(Fd, "~w~w~w~w~w~p~n",
		    [MegaSec,Sec, MicroSec,Action, Id, Event]),
    Fd;
handle_event(_, Fd) -> Fd.


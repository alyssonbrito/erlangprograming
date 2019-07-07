% Examples from Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Example 5.9. io_handler : Example of handler that uses the event_manager

-module(io_handler).
-export([init/1, terminate/1, handle_event/2]).

% Need on all event handlers
init(Count) -> Count.

% Need on all event handlers
% called on delete_handler/2 and stop/1
terminate(Count) -> {count, Count}.

%% Main handler
handle_event( { raise_alarm, Id, Alarm  }, Count  ) ->
    print(alarm, Id, Alarm, Count),
    Count+1;
handle_event( { clear_alarm, Id, Alarm  }, Count  ) ->
    print(clear, Id, Alarm, Count),
    Count+1;
handle_event( _Event, Count  ) ->
    Count.


%% internal
print(Type, Id, Alarm, Count) ->
    Date = fmt(date()),
    Time = fmt(time()),
    io:format("#~w,~s,~s,~w,~w,~p,~n",
	     [Count, Date, Time, Type, Id, Alarm]).

fmt({AInt, BInt, CInt}) ->
    AStr = pad(integer_to_list(AInt)),
    BStr = pad(integer_to_list(BInt)),
    CStr = pad(integer_to_list(CInt)),
    [AStr, $:, BStr, $:, CStr].

pad([M1]) -> [$0, M1];
pad(Other) -> Other.


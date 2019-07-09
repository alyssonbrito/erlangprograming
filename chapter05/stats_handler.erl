%% @author Alysson Brito
%% @reference Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
%% @doc Exercise 5.4 Event Statistics
%% NOTE: there is a problem on the exercise description:
%% it mentions first and second parameter, and later on mention the first and third.

-module(stats_handler).
-export([init/1, terminate/1, handle_event/2]).

% Need on all event handlers
init(EventHistory) -> EventHistory.

%% Event History
% Event History is a list of elements
%   `{{Type, Id}, Count}`
% Where the `Key` is `{Type,Id}`
% and Value is the number of times this event has happened before

% Need on all event handlers
% called on delete_handler/2 and stop/1
terminate(EventHistory) -> EventHistory.

%% @doc Main handler
% handle_event( { Type, Id, Alarm  }, EventHistory  ) when length(EventHistory) == 0->
%    [{ { Type, Id}, 0 } | EventHistory];
%% @TODO: use keytake ?
handle_event( { Type, Id, _Alarm  }, EventHistory  ) ->
    Key = {Type,Id},
    %% if event
    case lists:keysearch(Key, 1 , EventHistory) of
        false -> 
            io:format("[stats_handler][handler_event] Adding to list~n"),
            % ... is not on the list, 
            % add it to list with with counter 1.
            [{Key, 1 }] ++ EventHistory;
	    _Tuple -> 
            io:format("[stats_handler][handler_event] Found on list ~p~n",[EventHistory]),
            % ... if it is, increment counter
            % instead of remove it and add again, 
            % go trough list only once with map
            Fun = fun({TKey, Counter}) -> 
                    if TKey == Key -> 
                        {TKey, Counter+1};
                    true ->
                        {TKey, Counter}
                    end
                end,
            L = lists:map(Fun,EventHistory),
            io:format("[stats_handler][handler_event] New List ~p~n",[L]),
            L
    end. %case 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% f(). c(event_manager_53). c(stats_handler).
% event_manager_53:start(alarm, [{stats_handler, []}]).
% event_manager_53:send_event(alarm, {raise_alarm, 10, cabit_open}).
% event_manager_53:add_handler(alarm, io_handler, 1).
% event_manager_53:send_event(alarm, {clear_alarm, 10, cabit_open}).
% event_manager_53:swap_handlers(alarm, log_handler, log_handler).
% event_manager_53:send_event(alarm, {event, 156, link_up}).
% event_manager_53:get_data(alarm, io_handler).
% event_manager_53:delete_handler(alarm, io_handler).
% event_manager_53:delete_handler(alarm, crazy_name).
% event_manager_53:stop(alarm).


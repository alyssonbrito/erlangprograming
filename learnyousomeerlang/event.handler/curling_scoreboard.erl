%% @reference Learn You Some Erlang for great good
%% Event handler
%% Curling Time !

-module(curling_scoreboard).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
        terminate/2]).

init([]) ->
    {ok, []}.

handle_event({set_teams, TeamA, TeamB}, State) ->
    curling_scoreboard_hw:set_teams(TeamA, TeamB),
    {ok, State};
handle_event({add_points, Team, N}, State) ->
    [curling_scoreboard_hw:add_point(Team) || _ <- lists:seq(1,N)],
    {ok, State};
handle_event(next_round, State) ->
    curling_scoreboard_hw:next_round(),
    {ok, State};
handle_event(reset_board, State) ->
    curling_scoreboard_hw:reset_board(),
    {ok, State};
handle_event(_, State) ->
    {ok, State}.


handle_call(_, State) ->
    {ok, ok, State}.

handle_info(_, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.



% c(curling_scoreboard_hw). c(curling_scoreboard).
% {ok, Pid} = gen_event:start_link().
% gen_event:add_handler(Pid, curling_scoreboard, []).
% gen_event:notify(Pid, {set_teams, "Pirates", "Scotsmen"}).
% gen_event:notify(Pid, {add_points, "Pirates", 3}).
% gen_event:notify(Pid, next_round).
% gen_event:notify(Pid, reset_board).
% gen_event:delete_handler(Pid, curling_scoreboard, turn_off).




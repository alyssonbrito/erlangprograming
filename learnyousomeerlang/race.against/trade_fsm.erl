%% @reference Learn You Some Erlang for great good
%% Race agaist the finite-state machine
%% Trade game

-module(trade_fsm).

%% -----------------------------------------------------------
%% OTP State Machine Interface
-behavior(gen_fsm).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
	terminate/3, code_change/4,
	% custom state names
	idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2,
	negotiate/3, wait/2, ready/2, ready/3]).

%% -----------------------------------------------------------
%% Public APIs
%% -----------------------------------------------------------

-export([start/1, start_link/1, trade/2, accept_trade/1,
	make_offer/2, retract_offer/2, ready/1, cancel/1]).

start(Name) ->
    gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
    gen_fsm:start_link(?MODULE, [Name], []).

%% ask for a begin session.
trade(OwnPid, OtherPid) ->
    gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).

%% accept someone trade offer
accept_trade(OwnPid) ->
    gen_fsm:send_event(OwnPid, accept_negotiate). % ASYNC

%% send an item
make_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {make_offer, Item}).

%% Cancel trade
retract_offer(OwnPid, Item) ->
    gen_fsm:send_event(OwnPid, {retract_offer, Item}).

%% Mention that u r 4 a trade.
ready(OwnPid) ->
    gen_fsm:sync_send_event(OwnPid, ready, infinity).

%% Cancel the transaction
cancel(OwnPid) ->
    gen_fsm:sync_send_all_state_event(OwnPid, cancel). % Global

%% -----------------------------------------------------------
%% FSM TO FSM
%% -----------------------------------------------------------

ask_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).
%% Fw the client message accepting the transaction
accept_negotiate(OtherPid, OwnPid) ->
    gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).

%% fw a client's offer
do_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {do_offer, Item}).
%% fw a clients offer cancellation
undo_offer(OtherPid, Item) ->
    gen_fsm:send_event(OtherPid, {undo_offer, Item}).

% -- Going to ready state

%% Ask the other side if she's ready
are_you_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, are_you_ready).

%% Reply not ready
not_yet(OtherPid) ->
    gen_fsm:send_event(OtherPid, not_yet).

%% Waiting for ready state.
am_ready(OtherPid) ->
    gen_fsm:send_event(OtherPid, 'ready!').

% -- Ready state stuff

%% Ack that the fsm is in a ready state
ack_trans(OtherPid) ->
    gen_fsm:send_event(OtherPid, ack).

%% Ask if ready to commit
ask_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, ask_commit).

%% begin the sync commit
do_commit(OtherPid) ->
    gen_fsm:sync_send_event(OtherPid, do_commit).

% -- Cancel the trade
notify_cancel(OtherPid) ->
    gen_fsm:send_all_state_event(OtherPid, cancel).

%% ------------------------------------------------------------
%% GEN FS CALLBACKS
%% ------------------------------------------------------------

-record(state,{name = "", % ower name
	      other,
	      ownitems = "",
	      otheritems = "",
	      monitor,
	      from}).

init(Name) ->
    {ok, idle, #state{ name = Name }}.

idle({ask_negotiate, OtherPid}, State = #state{}) ->
    Ref = monitor(process, OtherPid),
    notice(State, "~p asked for a trade negotiation", [OtherPid]),
    {next_state, idle_wait, State#state{other = OtherPid, monitor = Ref}};
idle(Event, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

idle({negotiate, OtherPid}, From, State = #state{}) ->
    ask_negotiate(OtherPid, self()),
    notice(State, "asking user ~p for a trade", [OtherPid]),
    Ref = monitor(process, OtherPid),
    {next_state, idle_wait, State#state{other = OtherPid, monitor = Ref,
				       from = From}};
idle(Event, _From, Data) ->
    unexpected(Event, idle),
    {next_state, idle, Data}.

idle_wait({ask_negotiate, OtherPid}, State = #state{other = OtherPid}) ->
    gen_fsm:reply(State#state.from, ok),
    notice(State, "starting negotiation", []),
    {next_state, negotiate, State};
% the other side accepted negotiation
idle_wait({accept_negotiate, OtherPid}, State = #state{other = OtherPid}) ->
    gen_fsm:reply(State#state.from, ok),
    notice(State, "starting negotiation", []),
    {next_state, negotiation, State};
idle_wait(Event, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.


idle_wait(accepted_negotiate, _From, State = #state{other = OtherPid}) ->
    accept_negotiate(OtherPid, self()),
    notice(State, "accepting negotiation", []),
    {reply, ok, negotiate, State};
idle_wait(Event, _From, Data) ->
    unexpected(Event, idle_wait),
    {next_state, idle_wait, Data}.

negotiate({make_offer, Item}, S=#state{ownitems=OwnItems}) ->
    do_offer(S#state.other, Item),
    notice(S, "offering ~p", [Item]),
    {next_state, negotiate, S#state{ownitems=add(Item, OwnItems)}};
%% Own side retracting an item offer
negotiate({retract_offer, Item}, S=#state{ownitems=OwnItems}) ->
    undo_offer(S#state.other, Item),
    notice(S, "cancelling offer on ~p", [Item]),
    {next_state, negotiate, S#state{ownitems=remove(Item, OwnItems)}};
%% other side offering an item
negotiate({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
    notice(S, "other player offering ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
%% other side retracting an item offer
negotiate({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
    notice(S, "Other player cancelling offer on ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};
%% R U READY
negotiate(are_you_ready, S=#state{other=OtherPid}) ->
    io:format("Other user ready to trade.~n"),
    notice(S,
             "Other user ready to transfer goods:~n"
              "You get ~p, The other side gets ~p",
              [S#state.otheritems, S#state.ownitems]),
     not_yet(OtherPid),
     {next_state, negotiate, S};
negotiate(Event, Data) ->
     unexpected(Event, negotiate),
     {next_state, negotiate, Data}.
%% Ready state
negotiate(ready, From, S = #state{other=OtherPid}) ->
    are_you_ready(OtherPid),
    notice(S, "asking if ready, waiting", []),
    {next_state, wait, S#state{from=From}};
negotiate(Event, _From, S) ->
    unexpected(Event, negotiate),
    {next_state, negotiate, S}.

wait({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
    gen_fsm:reply(S#state.from, offer_changed),
    notice(S, "other side offering ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
wait({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
    gen_fsm:reply(S#state.from, offer_changed),
    notice(S, "Other side cancelling offer of ~p", [Item]),
    {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};

wait(are_you_ready, S=#state{}) ->
    am_ready(S#state.other),
    notice(S, "asked if ready, and I am. Waiting for same reply", []),
    {next_state, wait, S};

wait(not_yet, S = #state{}) ->
    notice(S, "Other not ready yet", []),
    {next_state, wait, S};
wait('ready!', S=#state{}) ->
    am_ready(S#state.other),
    ack_trans(S#state.other),
    gen_fsm:reply(S#state.from, ok),
    notice(S, "other side is ready. Moving to ready state", []),
    {next_state, ready, S};
%% DOn't care about these!
wait(Event, Data) ->
    unexpected(Event, wait),
    {next_state, wait, Data}.


ready(ack, S=#state{}) ->
    case priority(self(), S#state.other) of
	true ->
	    try
		notice(S, "asking for commit", []),
		ready_commit = ask_commit(S#state.other),
		notice(S, "ordering commit", []),
		ok = do_commit(S#state.other),
		notice(S, "committing...", []),
		commit(S),
		{stop, normal, S}
	catch Class:Reason ->
	    %% abort! Either ready_commit or do_commit failed
	    notice(S, "commit failed", []),
	    {stop, {Class, Reason}, S}
	end;
    false ->
	{next_state, ready, S}
    end;
ready(Event, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data}.

ready(ask_commit, _From, S) ->
    notice(S, "replying to ask_commit", []),
    {reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
    notice(S, "committing...", []),
    commit(S),
    {stop, normal, ok, S};
ready(Event, _From, Data) ->
    unexpected(Event, ready),
    {next_state, ready, Data}.


commit(S = #state{}) ->
    io:format("Transaction completed for ~s. "
        "Items sent are:~n~p,~n received are:~n~p.~n"
        "This operation should have some atomic save "
        "in a database.~n",
    [S#state.name, S#state.ownitems, S#state.otheritems]).


%% The other player has sent this cancel event
%% stop whatever we're doing and shut down!
handle_event(cancel, _StateName, S=#state{}) ->
    notice(S, "received cancel event", []),
    {stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

%% This cancel event comes from the client. We must warn the other
%% player that we have a quitter!
handle_sync_event(cancel, _From, _StateName, S = #state{}) ->
    notify_cancel(S#state.other),
    notice(S, "cancelling trade, sending cancel event", []),
    {stop, cancelled, ok, S};
%% Note: DO NOT reply to unexpected calls. Let the call-maker crash!
handle_sync_event(Event, _From, StateName, Data) ->
    unexpected(Event, StateName),
    {next_state, StateName, Data}.

handle_info({'DOWN', Ref, process, Pid, Reason}, _, S=#state{other=Pid, monitor=Ref}) ->
    notice(S, "Other side dead", []),
    {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
    unexpected(Info, StateName),
    {next_state, StateName, Data}.

%% Other methods
code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

%% Transaction completed.
terminate(normal, ready, S=#state{}) ->
    notice(S, "FSM leaving.", []);
    terminate(_Reason, _StateName, _StateData) ->
    ok.

%% Utilities
%% Send players a notice. This could be messages to their clients
%% but for our purposes, outputting to the shell is enough.
notice(#state{name=N}, Str, Args) ->
    io:format("~s: "++Str++"~n", [N|Args]).

%% Unexpected allows to log unexpected messages
unexpected(Msg, State) ->
    io:format("~p received unknown event ~p while in state ~p~n",
    [self(), Msg, State]).

priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid -> false.


%% Items management
add(Item, Items) ->
    [Item | Items].

remove(Item, Items) ->
    Items -- [Item].


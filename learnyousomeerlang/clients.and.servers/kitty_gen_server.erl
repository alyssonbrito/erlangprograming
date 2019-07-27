%% @reference Learn You Some Erlang for great good
%% Clients and servers
%% Version using gen_serve of kitty_server

-module(kitty_gen_server).
-behavior(gen_server).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(cat, {name, color = green, description}).

%% Client API
%start_link() -> spawn_link(fun init/0).
start_link() ->
    % Starts a loop with this initial state
    %my_server:start_link(?MODULE, []).
    % (module, params to sent to init(), debugging_options)
    gen_server:start_link(?MODULE, [], []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    % Sync call with (Pid, Msg)
    %my_server:call(Pid, {order, Name, Color, Description} ).
    % a third parameter can be tiemout
    gen_server:call(Pid, {order, Name, Color, Description} ).

% Asynchronous
return_cat(Pid, Cat = #cat{}) ->
    %my_server:cast(Pid, {return, Cat}).
    gen_server:cast(Pid, {return, Cat}).

% Synchronous
close_shop(Pid) ->
     %my_server:call(Pid, terminate).
     gen_server:call(Pid, terminate).

%% Called from the my_server
init([]) ->
    %[].
    {ok, []}.

handle_call({order, Name, Color, Description}, From, Cats) ->
    if Cats =:= [] ->
	   %my_server:reply(From, make_cat(Name, Color, Description)),
	   %Cats;
	   {reply, make_cat(Name, Color, Description), Cats};
       Cats =/= [] -> % empty stock
	   %my_server:reply(From, hd(Cats)),
           %tl(Cats)
	   {reply, hd(Cats), tl(Cats)}
    end;
handle_call(terminate, From, Cats) ->
    %my_server:reply(From, ok),
    %terminate(Cats).
    {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->
    %[Cat|Cats].
    {noreply, [Cat|Cats] }.

handle_info(Msg, Cats) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, Cats}.

terminate(normal, Cats) ->
    [ io:format("~p was set free. ~n",[C#cat.name]) || C <- Cats ],
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.

%% Private functions
make_cat(Name, Color, Description) ->
    #cat{ name = Name, color = Color, description = Description }.


% c(kitty_gen_server).
% rr(kitty_gen_server).
% {ok, Pid} = kitty_gen_server:start_link().
% Pid ! <<"Test handle_info">>.
% Cat = kitty_gen_server:order_cat(Pid, "Cat Stevens", white, "not actually a cat").
% kitty_gen_server:return_cat(Pid, Cat).
% kitty_gen_server:order_cat(Pid, "Kitten Mittens", black, "look at them little paws!").
% kitty_gen_server:order_cat(Pid, "Kitten Mittens", black, "look at them little paws!").
% kitty_gen_server:return_cat(Pid, Cat).
% kitty_gen_server:close_shop(Pid).



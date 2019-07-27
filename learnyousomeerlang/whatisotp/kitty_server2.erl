%% @reference Learn You Some Erlang for great good
%% What is OTP ?
%% Version 2 using my_serve module

-module(kitty_server2).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(cat, {name, color = green, description}).

%% Client API
%start_link() -> spawn_link(fun init/0).
start_link() ->
    % Starts a loop with this initial state
    my_server:start_link(?MODULE, []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    % Sync call with (Pid, Msg)
    my_server:call(Pid, {order, Name, Color, Description} ).

% Asynchronous
return_cat(Pid, Cat = #cat{}) ->
    my_server:cast(Pid, {return, Cat}).

% Synchronous
close_shop(Pid) ->
     my_server:call(Pid, terminate).

%% Called from the my_server
init([]) -> [].

handle_call({order, Name, Color, Description}, From, Cats) ->
    if Cats =:= [] ->
	   my_server:reply(From, make_cat(Name, Color, Description)),
	   Cats;
       Cats =/= [] -> % empty stock
	   my_server:reply(From, hd(Cats)),
           tl(Cats)
    end;
handle_call(terminate, From, Cats) ->
    my_server:reply(From, ok),
    terminate(Cats).

handle_cast({return, Cat = #cat{}}, Cats) ->
    [Cat|Cats].

%% Private functions
make_cat(Name, Color, Description) ->
    #cat{ name = Name, color = Color, description = Description }.

terminate(Cats) ->
    [ io:format("~p was set free. ~n",[C#cat.name]) || C <- Cats ],
    exit(normal).

% c(kitty_server2). c(my_server).
% rr(kitty_server2).
% Pid = kitty_server2:start_link().
% Cat1 = kitty_server2:order_cat(Pid, carl, brown, "loves to burn bridges").
% kitty_server2:return_cat(Pid, Cat1).
% kitty_server2:order_cat(Pid, jimmy, orange, "cuddly").
% kitty_server2:order_cat(Pid, jimmy, orange, "cuddly").
% kitty_server2:return_cat(Pid, Cat1).
% kitty_server2:close_shop(Pid).
% kitty_server2:close_shop(Pid).


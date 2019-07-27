%% @reference Learn You Some Erlang for great good
%% What is OTP ?

%% So this is a kitty server/store. The behavior is extremely simple:
%% you describe a cat and you get that cat. If someone returns a cat,
%% it's added to a list and is then automatically sent as the next order
%% instead of what the client actually asked for (we're in this kitty
%% store for the money, not smiles):

-module(kitty_server).
-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, {name, color = green, description}).

%% Client API
start_link() -> spawn_link(fun init/0).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! { self(), Ref, {order, Name, Color, Description} },
    receive
	{Ref, Cat} ->
	    erlang:demonitor(Ref, [flush]),
	    Cat;
	{'DOWN', Ref, process, Pid, Reason} ->
	    erlang:error(Reason)
    after 5000 -> erlang:error(timeout)
    end.

% Asynchronous
return_cat(Pid, Cat = #cat{}) ->
    Pid ! {return, Cat},
    ok.

% Synchronous
close_shop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, terminate},
    receive
	{Ref, ok} ->
	    erlang:demonitor(Ref, [flush]),
	    ok;
	{'DOWN', Ref, process, Pid, Reason} ->
	    erlang:error(Reason)
    after 5000 -> erlang:error(timeout)
    end.

%% Server function
init() -> loop([]).

loop(Cats) ->
    receive
	{Pid, Ref, {order, Name, Color, Description}} ->
	    if Cats =:= [] ->
		   Pid ! {Ref, make_cat(Name, Color, Description)},
		   loop(Cats);
	       Cats =/= [] -> % empty stock
		   Pid ! {Ref, hd(Cats)},
	           loop(tl(Cats))
	    end;
	{return, Cat = #cat{}} ->
	    loop([Cat|Cats]);
	{Pid, Ref, terminate} ->
	    Pid ! {Ref, ok},
	    terminate(Cats);
	Unknown ->
	    % do some logging
	    io:format("Unknown message: ~p~n",[Unknown]),
	    loop(Cats)
    end.

%% Private functions
make_cat(Name, Color, Description) ->
    #cat{ name=Name, color = Color, description = Description }.

terminate(Cats) ->
    [ io:format("~p was set free. ~n",[C#cat.name]) || C <- Cats ],
    ok.


% c(kitty_server).
% rr(kitty_server).
% Pid = kitty_server:start_link().
% Cat1 = kitty_server:order_cat(Pid, carl, brown, "loves to burn bridges").
% kitty_server:return_cat(Pid, Cat1).
% kitty_server:order_cat(Pid, jimmy, orange, "cuddly").
% kitty_server:order_cat(Pid, jimmy, orange, "cuddly").
% kitty_server:return_cat(Pid, Cat1).
% kitty_server:close_shop(Pid).
% kitty_server:close_shop(Pid).


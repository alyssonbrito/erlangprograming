-module(mph_drop).
-export([mph_drop/0]).

mph_drop()->
    io:format("[mph_drop][mph_drop]~n"),
    process_flag(trap_exit,true),
    Drop = spawn_link(drop,drop,[]),
    convert(Drop).

convert(Drop) ->
    io:format("[mph_drop][convert] WAITING ~p... ~n",[self()]),
    receive
	{'EXIT', Pid, Reason} ->
	    io:format("CCCCCCRRRRRRRAAASSSSSSSS Pid: [~p] Reason[~p].~n",
		     [Pid, Reason]),
	    NewDrop = spawn_link(drop,drop,[]),
	    convert(NewDrop);
	{Planemo, Distance} ->
	    io:format("[mph_drop][convert] ... receive1~n"),
	    Drop ! {self(), Planemo, Distance},
	    convert(Drop);
	{Planemo, Distance, Velocity} ->
	    io:format("[mph_drop][convert] ... receive2~n"),
	    MphVeco = 2.23693629 * Velocity,
	    io:format("On ~p, a fall of ~p meters yields a velocity of ~p mph.~n",
		     [Planemo, Distance, MphVeco]),
	    convert(Drop)

    end.

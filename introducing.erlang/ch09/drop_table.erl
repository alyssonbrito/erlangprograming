-module(drop_table).
-export([drop/0]).
-include("records.hrl").

drop() ->
    setup(),
    handle_drops().

handle_drops() ->
    receive
	{From, Planemo, Distance} ->
	    From ! {Planemo, Distance, fall_velocity(Planemo, Distance)},
	    handle_drops();
	_ ->
	    io:format("Wrong Paramter~n")
    end.

fall_velocity(Planemo, Distance) when Distance >= 0 ->
    P = hd ( ets:lookup(planemos, Planemo)  ),
    math:sqrt(2 * P#planemo.gravity * Distance).

setup() ->
    PlanemoTable = ets:new(planemos, [named_table, {keypos,#planemo.name}]),
    ets:insert(planemos, #planemo{ name=mercury, gravity = 3.7, diameter = 4878, distance_from_sun = 57.9}),
    ets:insert(planemos, #planemo{ name=venus, gravity = 8.9, diameter = 12104, distance_from_sun = 108.2}),
    ets:insert(PlanemoTable, #planemo{ name=earth, gravity = 9.8, diameter = 12756, distance_from_sun = 149.6}),
    ets:insert(PlanemoTable, #planemo{ name=moon, gravity = 1.6, diameter = 3475, distance_from_sun = 149.6}),
    ets:insert(PlanemoTable, #planemo{ name=ceres, gravity = 0.27, diameter = 950, distance_from_sun = 413.7}),
    ets:insert(PlanemoTable, #planemo{ name=jupiter, gravity = 23.1, diameter = 142796, distance_from_sun = 778.3}),
    ets:insert(PlanemoTable, #planemo{ name=saturn, gravity = 9.0,   diameter = 120660, distance_from_sun = 1427.0}),
    ets:insert(PlanemoTable, #planemo{ name=uranus, gravity = 8.7,   diameter = 51118, distance_from_sun = 2871.0}),
    ets:insert(PlanemoTable, #planemo{ name=neptune, gravity = 11.0,   diameter = 30200, distance_from_sun = 4497.1}),
    ets:insert(PlanemoTable, #planemo{ name=mars, gravity = 3.7,     diameter = 6787, distance_from_sun = 227.9}).



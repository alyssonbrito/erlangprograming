-module(planemo_storage).
-export([setup/0,populate/1,info/0,info/1, read/1]).
-include("records.hrl").

setup() ->
    PlanemoTable = ets:new(planemos, [named_table, {keypos,#planemo.name}]),
    PlanemoTable.

populate(PlanemoTable) ->
    ets:insert(planemos, #planemo{ name=mercury, gravity = 3.7, diameter = 4878, distance_from_sun = 57.9}),
    ets:insert(planemos, #planemo{ name=venus, gravity = 8.9, diameter = 12104, distance_from_sun = 108.2}),
    ets:insert(PlanemoTable, #planemo{ name=earth, gravity = 9.8, diameter = 12756, distance_from_sun = 149.6}),
    ets:insert(PlanemoTable, #planemo{ name=moon, gravity = 1.6, diameter = 3475, distance_from_sun = 149.6}),
    ets:insert(PlanemoTable, #planemo{ name=ceres, gravity = 0.27, diameter = 950, distance_from_sun = 413.7}),
    ets:insert(PlanemoTable, #planemo{ name=jupiter, gravity = 23.1, diameter = 142796, distance_from_sun = 778.3}),
    ets:insert(PlanemoTable, #planemo{ name=mars, gravity = 3.7, diameter = 6787, distance_from_sun = 227.9}).

info() ->
    ets:info(planemos).

info(TableNAme) ->
    ets:info(TableNAme),
    ets:tab2list(TableNAme).


read(TableNAme) ->
    ets:tab2list(TableNAme).



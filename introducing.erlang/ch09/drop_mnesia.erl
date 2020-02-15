-module(drop_mnesia).
-export([setup/0]).
-export([fall_velocity/2]).
-export([transaction/0 , transaction/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("records.hrl").

transaction()->
    F = fun() -> qlc:eval(
		   qlc:q( [ X || X <- mnesia:table(planemo)  ]  )
		  )end,
    mnesia:transaction(F).

transaction(MaxGravity)->
    F = fun() -> qlc:eval(
		   qlc:q( [ {X#planemo.name, X#planemo.gravity} || X <- mnesia:table(planemo),
				 X#planemo.gravity < MaxGravity  ]  )
		  )end,
    mnesia:transaction(F).


fall_velocity(Planet, Distance) ->
    {atomic, [P | _]} = mnesia:transaction (fun() ->  mnesia:read(planemo, Planet)  end),
    math:sqrt(2 * P#planemo.gravity * Distance).

setup() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(planemo, [{attributes, record_info(fields, planemo)}]),
    F = fun() ->
		mnesia:write(#planemo{ name=mercury, gravity = 3.7, diameter = 4878, distance_from_sun = 57.9}),
		mnesia:write(#planemo{ name=venus, gravity = 8.9, diameter = 12104, distance_from_sun = 108.2}),
		mnesia:write(#planemo{ name=earth, gravity = 9.8, diameter = 12756, distance_from_sun = 149.6}),
		mnesia:write(#planemo{ name=moon, gravity = 1.6, diameter = 3475, distance_from_sun = 149.6}),
		mnesia:write(#planemo{ name=ceres, gravity = 0.27, diameter = 950, distance_from_sun = 413.7}),
		mnesia:write(#planemo{ name=jupiter, gravity = 23.1, diameter = 142796, distance_from_sun = 778.3}),
		mnesia:write(#planemo{ name=saturn, gravity = 9.0,   diameter = 120660, distance_from_sun = 1427.0}),
		mnesia:write(#planemo{ name=uranus, gravity = 8.7,   diameter = 51118, distance_from_sun = 2871.0}),
		mnesia:write(#planemo{ name=neptune, gravity = 11.0,   diameter = 30200, distance_from_sun = 4497.1}),
		mnesia:write(#planemo{ name=mars, gravity = 3.7,     diameter = 6787, distance_from_sun = 227.9})
	end,
    mnesia:transaction(F).



% handle_drops() ->
%     receive
% 	{From, Planemo, Distance} ->
% 	    %From ! {Planemo, Distance, fall_velocity(Planemo, Distance)},
% 	    handle_drops();
% 	_ ->
% 	    io:format("Wrong Paramter~n")
%     end.

% fall_velocity(Planemo, Distance) when Distance >= 0 ->
%    P = hd ( ets:lookup(planemos, Planemo)  ),
%    math:sqrt(2 * P#planemo.gravity * Distance).


-module(record_drop).
-export([fall_velocity/1]).

-include("records.hrl").

% V1: Pattern match the whole #tower
% fall_velocity(#tower{} = T) ->
%     fall_velocity(T#tower.planemo,T#tower.height).

% V2: PAttern match with the field only
% fall_velocity(#tower{planemo= Planemo, height=Distance}) ->
%     fall_velocity(Planemo,Distance).

% V3: Pattern match #tower and fields!
fall_velocity(#tower{planemo= Planemo, height=Distance} = T) ->
    Valor = fall_velocity(Planemo, Distance),

    io:format("From ~s 's elevation of ~p meters on ~p, the object will reach ~p m/s before crashing in ~s.~n",
	     [T#tower.name, T#tower.height, Planemo, Valor, T#tower.location]).

fall_velocity(Planemo, Distance) when Distance >= 0->
    Gravity = case Planemo of
		  earth -> 9.8;
		  moon -> 1.6;
		  mars -> 3.71
	      end,
    math:sqrt(2 * Gravity * Distance);
fall_velocity(_,_) ->
    io:format("Invalid input!!!~n").

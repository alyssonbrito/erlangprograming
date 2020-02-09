%% @author Alysson Brito <alyssonbrito@gmail.com> [http://alysson.com]
%% @doc Functions calculating velocies achieved by objects
%% dropped in a vaccum.
%% @reference fronm <a href="http://link.com">
%% Introductiong Erlang </a>
%% O'Relly Media, Inc. ,2017.
%% @copyright 2016 by Simon St. Laurent
%% @version 0.1
%%

-module(drop).
-export([fall_velocity/2]).

%% @doc Calculates the volicity of an objetc falling on Earth
%% as if it were in a vacuum (no air resistance). The distance is
%% the height from which the objet falls specified in meters
%% and the functino returns a velocity in meters per second
%-spec(fall_velocity(number()) -> number()).
fall_velocity(earth, Distance) -> math:sqrt(2 * 9.8 * Distance);
fall_velocity(moon, Distance) -> math:sqrt(2 * 1.6 * Distance);
fall_velocity(mars, Distance) -> math:sqrt(2 * 3.71 * Distance).



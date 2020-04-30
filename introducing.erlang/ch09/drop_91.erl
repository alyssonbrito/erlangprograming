-module(drop_91).
-export([fall_velocity/2]).

fall_velocity(PlanetName, Distance) ->
    Gravity = case PlanetName of
        earth -> 9.8;
        moon -> 1.6;
        mars -> 3.71
    end,

    try math:sqrt(2 * Gravity * Distance) of
        Result -> Result
    catch
        error:Error -> {error, Error}
    end.

%% try above same as 
%% try math:sqrt(2 * Gravity * Distance)
%% catch
%%     error:Error -> {error, Error}
%% end.

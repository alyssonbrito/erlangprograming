-module(drop_unsup).
-export([fall_velocity/2]).

fall_velocity(Planet, Distance) ->
    try
	Gravity = case Planet of
		      earth -> 9.8;
		      moon -> 1.6;
		      mars -> 3.71
		  end,
	math:sqrt(2*Gravity*Distance)
    of
	Result -> Result
    catch
	error:Error -> {error, Error}
    end.

%% same as
%%    try math:sqrt(2*Gravity*Distance)
%%    catch
%%	error:Error -> {error, Error}
%%    end.


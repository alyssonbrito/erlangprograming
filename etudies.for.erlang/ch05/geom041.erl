%% @doc Solution to Étude 4.1 Using case
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.1

-module(geom041).
-export([area/3]).

%% @doc Calculates the area of given rectangle
%% given the height and width. Returns calculated area
%% @param atom Either rectangle, triangle or ellipse
%% @param A Side A
%% @param B Side B
-spec(area(atom(),number(),number()) -> number()).
area(Shape,A,B) when A>=0, B>=0 ->
    Result = case Shape of
		 rectangle -> A*B;
		 ellipse -> A*B*math:pi();
		 triangle -> (A*B)/2
	     end,
    Result.


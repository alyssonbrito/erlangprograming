%% @doc Solution to Étude 3.1 Pattern Matching
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.3

-module(geom031).
-export([area/3]).

%% @doc Calculates the area of given rectangle
%% given the height and width. Returns calculated area
%% @param atom Either rectangle, triangle or ellipse
%% @param A Side A
%% @param B Side B
-spec(area(atom(),number(),number()) -> number()).
area(rectangle,A,B) -> A*B;
area(ellipse,A,B) -> A*B*math:pi();
area(triangle,A,B) -> (A*B)/2.


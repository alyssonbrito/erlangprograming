%% @doc Solution to Étude 3.3 Underscores
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.5

-module(geom033).
-export([area/3]).

%% @doc Calculates the area of given rectangle
%% given the height and width. Returns calculated area
%% @param atom Either rectangle, triangle or ellipse
%% @param A Side A
%% @param B Side B
-spec(area(atom(),number(),number()) -> number()).
area(rectangle,A,B) when A>=0, B>=0 -> A*B;
area(ellipse,A,B) when A>=0, B>=0 -> A*B*math:pi();
area(triangle,A,B) when A>=0, B>=0 -> (A*B)/2;
area(_,_,_) -> 0.


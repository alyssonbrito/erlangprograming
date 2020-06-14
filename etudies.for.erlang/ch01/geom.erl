%% @doc Solution to Étude 2.1 Writing a Function
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.2

-module(geom).
-export([area/2]).

%% @doc Calculates the area of given rectangle
%% given the height and width. Returns calculated area
-spec(area(number(),number()) -> number()).
area(H,W) -> H*W.


%% @doc Solution to Étude 5.2 Using the re Module
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.1

-module(dates052).
-export([date_parts/1]).


%% http://docs.zotonic.com/en/latest/cookbook/justenough-re.html
%% @doc Extract the date information from a ISO string
%% into a its number components
%% @param IsoDateString  ISO date format ("yyyy-mm-dd")
%% @return [yyyy, mm, dd]
-spec(date_parts(string()) -> list()).

date_parts(IsoDateString) -> 
    [Y,M,D] = re:split(IsoDateString, "[-]", [{return, list}]),
    [list_to_integer(Y),list_to_integer(M),list_to_integer(D)].


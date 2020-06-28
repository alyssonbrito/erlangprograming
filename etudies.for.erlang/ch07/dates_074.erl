%% @doc Solution to Étude 7.4: Using lists:split/2
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.2

-module(dates_074).
-export([date_parts/1, new_julian/1, julian/1]).

%% @doc Given a date in ISO string
%% an return then day of the year
%% @param IsoDateString  ISO date format ("yyyy-mm-dd")
%% @return Number
-spec(new_julian(string()) -> number()).
new_julian(IsoDateString) -> 
    [Y,M,D] = date_parts(IsoDateString),
    DaysPerMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
    {MonthsIn, _} = lists:split(M -1, DaysPerMonth),
    TotalIn = lists:foldl( fun(Days, Acc) -> Days + Acc end, 0, MonthsIn ),
    case M > 2 andalso is_leap_year(Y) of
        true -> TotalIn + D + 1;
        false -> TotalIn + D
    end.

%% @doc Given a date in ISO string
%% an return then day of the year
%% @param IsoDateString  ISO date format ("yyyy-mm-dd")
%% @return Number
-spec(julian(string()) -> number()).
julian(IsoDateString) -> 
    [Y,M,D] = date_parts(IsoDateString),
    DaysPerMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
    julian(Y, M, D, DaysPerMonth, 0).


julian(Y, M, D, DaysPerMonth, Acc) when M == (13 - length(DaysPerMonth)) -> 
    IsLeap = is_leap_year(Y),
    Leap = if
        IsLeap, M > 2 -> 1;
        true -> 0
    end,
    Acc + Leap + D;
julian(Y, M, D, DaysPerMonth, Acc) ->
    julian(Y, M, D, tl(DaysPerMonth), Acc + hd(DaysPerMonth)).


is_leap_year(Year) ->
    (Year rem 4 == 0 andalso Year rem 100 /= 0) orelse (Year rem 400 == 0).


%% http://docs.zotonic.com/en/latest/cookbook/justenough-re.html
%% @doc Extract the date information from a ISO string
%% into a its number components
%% @param IsoDateString  ISO date format ("yyyy-mm-dd")
%% @return [yyyy, mm, dd]
-spec(date_parts(string()) -> list()).

date_parts(IsoDateString) -> 
    [Y,M,D] = re:split(IsoDateString, "[-]", [{return, list}]),
    [list_to_integer(Y),list_to_integer(M),list_to_integer(D)].


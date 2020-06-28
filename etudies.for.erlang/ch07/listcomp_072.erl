%% @doc Solution to Étude 7.2 List Comprehensions and Pattern Matching
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.1

-module(listcomp_072).
-export([older_males/0, older_or_male/0]).

%% @doc Calculate derivative by aproximation definition.
%% (Fn(X + H) - Fn(X)) / H
-spec(older_males() -> list() ).
older_males() ->
    People = get_people(),
    [ {Name, Age} || {Name, Gender, Age} <- People, Gender == $M, Age > 40  ].

-spec(older_or_male() -> list() ).
older_or_male() ->
    People = get_people(),
    [ {Name, Age} || {Name, Gender, Age} <- People, (Gender == $M) orelse (Age > 40) ].

get_people() ->
      [{"Federico", $M, 22}, {"Kim", $F, 45}, {"Hansa", $F, 30},
      {"Vu", $M, 47}, {"Cathy", $F, 32}, {"Elias", $M, 50}].

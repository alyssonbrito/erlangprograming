%% @doc Solution to Étude 7.5: Multiple Generators in List Comprehensions
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.1

-module(cards_075).
-export([make_deck/0,test/0]).

%% @doc Given a date in ISO string
%% an return then day of the year
%% @param IsoDateString  ISO date format ("yyyy-mm-dd")
%% @return Number
 %% @doc generate a deck of cards
make_deck() ->
    [{X, Y} || X <- ["A", 2, 3, 4, 5, 6, 7, 8, 9, "J", "Q", "K"], Y <- ["Clubs", "Diamons", "Hearts", "Spades"]].

test() ->
    Deck = make_deck(),
    lists:foreach( fun(Item) -> io:format("[~p~n]", [Item]) end, Deck).
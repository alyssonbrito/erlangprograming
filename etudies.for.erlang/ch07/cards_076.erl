%% @doc Solution to Étude 7.6: Explaining an Algorithm
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.2

-module(cards_076).
-export([make_deck/0,test/0, shuffle/1]).

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

%% @doc Shuffer a dekc of cards
-spec(shuffle(list()) -> list()).
shuffle(List) -> shuffle(List, []).

shuffle([], SuffledDeck) -> SuffledDeck;
shuffle(List, SuffledDeck) ->
    % Splits the list in 2. Pick the middle element
    {FirstPart, [M | SecondPart]} = lists:split(rand:uniform(length(List)) - 1, List), 
    % Add the middle element to the final resul. S
    % Shuffle rest (first and second part)
    shuffle(FirstPart ++ SecondPart, [M | SuffledDeck]).

%% @doc Solution to Étude 6.5 Random Numbers; Generating Lists of Lists
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.1

-module(non_fp065).
-export([generate_teeth/2,gtest/0]).
-export([alert/1, test/0]).


gtest() ->
    _Arcada = "FFFFFFFFFTFFFFFFFFFTFFFFFFFFFFFF",
    Arc = "FTTTTTTTTTTTTTTFTTTTTTTTTTTTTTTT",
    A = generate_teeth(Arc,0.75),
    printa(A),
    A.

printa([]) -> ok;
printa([H|T]) ->
    io:format("~w~n",[H]),
    printa(T);
printa(A) ->
    io:format("GOT ~w~n",[A]).

-spec(generate_teeth(string(), float()) -> list(list(integer()))).
generate_teeth(TeethHere,Prob) ->
    generate_teeth(TeethHere,Prob,[]).

-spec(generate_teeth(string(), float(), [[integer()]]) -> [[integer()]]).
generate_teeth([],_Prob,Acc) ->  lists:reverse(Acc); % lists:reverse(Acc); Acc;
generate_teeth([$F | T], Prob, Acc)  ->
    generate_teeth(T, Prob, [[0] | Acc]);
generate_teeth([$T | T],Prob,Acc) ->
    generate_teeth(T, Prob, [ generate_tooth(Prob) | Acc]).

generate_tooth(Prob) ->
    IsGood = rand:uniform() < Prob,
    case IsGood of
        true -> BaseDepth = 2;
        false -> BaseDepth = 3
    end,
    generate_tooth(BaseDepth, 6, []).

generate_tooth_0(Prob) ->
    N = rand:uniform(2),
    Base = if
        N < Prob -> 2;
        true -> 3
    end,
    generate_tooth(Base,6,[]).

generate_tooth(_B,0,Acc) -> Acc;
generate_tooth(B,N,Acc) ->
    Adjust = get_random(),
    [B + Adjust | generate_tooth(B, N - 1, Acc)].


generate_tooth2(B,N,Acc) ->
    Adjust = get_random(),
    generate_tooth2(B , N-1, [Adjust + B | Acc]).

get_random() ->
    rand:uniform(3) - 2.

get_random2() ->
    Signal = case rand:uniform(2) of
        1 -> -1;
        2 -> 1
    end,
    rand:uniform(3)  * Signal.


test() ->
    PocketDepths = [[0], [2,2,1,2,2,1], [3,1,2,3,2,3],
    [3,1,3,2,1,2], [3,2,3,2,2,1], [2,3,1,2,1,1],
    [3,1,3,2,3,2], [3,3,2,1,3,1], [4,3,3,2,3,3],
    [3,1,1,3,2,2], [4,3,4,3,2,3], [2,3,1,3,2,2],
    [1,2,1,1,3,2], [1,2,2,3,2,3], [1,3,2,1,3,3], [0],
    [3,2,3,1,1,2], [2,2,1,1,3,2], [2,1,1,1,1,2],
    [3,3,2,1,1,3], [3,1,3,2,3,2], [3,3,1,2,3,3],
    [1,2,2,3,3,3], [2,2,3,2,3,3], [2,2,2,4,3,4],
    [3,4,3,3,3,4], [1,1,2,3,1,2], [2,2,3,2,1,3],
    [3,4,2,4,4,3], [3,3,2,1,2,3], [2,2,2,2,3,3],
    [3,2,3,2,3,2]],
    alert(PocketDepths).

%% @doc Takes a list of 32 lists of six numbers as its input.
%% If a tooth isn’t present, it is represented by [0]
%% an return then day of the year
%% @param Teeths  List of list 32,6
%% A non existent tooth is represented as []
%% @return Number
-spec(alert(list()) -> number()).
alert(Teeths) ->
    lists:reverse(checkTooths(Teeths,1,[])).

checkTooths([],_, Acc) -> Acc;
checkTooths(Teeths,Counter, Acc) ->
    case cheeckTeeth(hd(Teeths)) of
        true -> checkTooths(tl(Teeths), Counter+1, [  Counter | Acc ] );
        false -> checkTooths(tl(Teeths), Counter+1, Acc )
    end.

cheeckTeeth([]) -> false;
cheeckTeeth(TeethPockets) when hd(TeethPockets) >= 4 -> true;
cheeckTeeth(TeethPockets)  -> cheeckTeeth(tl(TeethPockets)).



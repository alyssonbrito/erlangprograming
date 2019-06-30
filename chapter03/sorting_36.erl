% author: alyssonbrito@gmail.com
% 2019.Jun
% Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Exercise 3.6


-module(sorting_36).
-export([qsort/1, mergesort/1,partition_list/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% interfaces
qsort(L) -> qsort_feio(L).
mergesort(L) -> mergesortd(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% qsort without advanced listing
qsort_feio([]) -> [];
qsort_feio([H | T]) ->
    qsort_feio(smaller(T,H)) ++ [H] ++ qsort_feio(bigger(T,H)).

% helper functions
smaller([], _N) -> [];
smaller([ H | T ], N) when H =< N -> [ H | smaller (T,N) ];
smaller([ _H | T ], N) -> smaller(T,N).

bigger([], _N) -> [];
bigger([ H | T ], N) when H > N -> [ H | bigger (T,N) ];
bigger([ _H | T ], N) -> bigger(T,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% qsort decent
qsortd([]) -> [];
qsortd([X|Xs]) ->
   qsortd([ Y || Y <- Xs, Y < X]) ++ [X] ++ qsortd([ Y || Y <- Xs, Y >= X]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% merge sort
%% break in two arrays, merge both in order
mergesortf([]) -> [];
mergesortf([H | []]) -> [H];
mergesortf(L) ->
    % at least 2 elements
    LSize = length(L),
    Half = (LSize div 2),
    L1 = mergesortf( partition_list(L,0,Half,0)),
    L2 = mergesortf( partition_list(L,Half,LSize,0)),
    merge_and_sort(L1,L2).

% helper functions
partition_list([],_A,_B,_I) -> [];
partition_list(_L,_A,B,I)  when I >= B -> [];
partition_list([_H|T],A,B,I) when I<A -> partition_list(T,A,B,I+1);
partition_list([H|T],A,B,I) -> [H] ++ partition_list(T,A,B,I+1).

merge_and_sort([],[]) -> [];
merge_and_sort(L,[]) -> L;
merge_and_sort([], L) -> L;
merge_and_sort([H1 | T1], [H2 | T2]) ->
    if
	H1 =< H2 -> [H1] ++ merge_and_sort(T1,[H2 | T2]);
	true    -> [H2] ++ merge_and_sort([H1|T1],T2)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mergesort decent using list module
mergesortd(L) when length(L) == 1 -> L;
mergesortd(L) when length(L) > 1 ->
    {L1, L2} = lists:split(length(L) div 2, L),
    lists:merge(mergesortd(L1), mergesortd(L2)).

%% lists:merge
%%  All these sub-lists must be sorted prior to evaluating this function.qsort



-module(listdrop).
-export([falls/1]).

falls(List) -> falls(List,[]).

%falls([], Results) -> Results;
falls([], Results) -> lists:reverse(Results);
falls([H|T], Results) -> falls(T, [drop:fall_velocity(H) | Results]).

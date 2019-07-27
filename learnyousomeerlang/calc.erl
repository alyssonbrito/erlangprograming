%% @reference Book Learn Your some Erlang for the great good!
%% Function solving problems

-module(calc).
-export([rpn/1,rpn_test/0]).

%% @doc Reverse Polish Notation calculator
%% Example: "10 4 3 + 2 * -"
rpn(L) when is_list(L) ->
    [ Res ] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    Res.

rpn("sum", [N | []]) -> [N];
rpn("+", [N1, N2 | S]) -> [N2+N1 | S];
rpn("-", [N1, N2 | S]) -> [N2-N1 | S];
rpn("*", [N1, N2 | S]) -> [N2*N1 | S];
rpn("/", [N1, N2 | S]) -> [N2/N1 | S];
rpn("^", [N1, N2 | S]) -> [math:pow(N2,N1) | S];
rpn("ln", [N | S]) -> [math:log(N) | S];
rpn("log10", [N | S]) -> [math:log10(N) | S];
rpn("sum", [N1, N2 | S]) -> rpn("sum", [N1+N2 | S]);
rpn("prod", [N1, N2 | S]) -> [ N2*N1 | S];
rpn(X, Stack) -> [read(X) | Stack].

%% @doc Convert number (float or integer) from string format to correct
%% format
read(N) ->
    case string:to_float(N) of
        {error, no_float} -> list_to_integer(N);
        {F, _} -> F
    end.


rpn_test() ->
    5 = rpn("2 3 +"),
    87 = rpn("90 3 -"),
    -4 = rpn("10 4 3 + 2 * -"),
    -2.0 = rpn("10 4 3 + 2 * - 2 /"),
    ok = try 
        rpn("90 34 12 33 55 66 + * - *")
        catch
            error:{badmatch, [_|_]} -> ok
        end,
    4037 = rpn("90 34 12 33 55 66 + * - + -"),
    true = math:sqrt(2) == rpn("2 0.5 ^"),
    50 = rpn("10 10 10 20 sum"),
    ok.


% Test cases:
% c(calc).
% calc:rpn("3 5 +").  % 8
% calc:rpn("7 3 + 5 +").  %15
% 
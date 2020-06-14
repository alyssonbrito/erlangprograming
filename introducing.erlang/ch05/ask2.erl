-module(ask2).
-export([term/0, chars/0, line/0]).

line() ->
    Planemo = get_planemo(),
    Distance = get_distance(),
    drop:fall_velocity({Planemo, Distance}).

get_planemo() ->
    io:format("Where are you?~n"),
    io:format(" 1. Earth ~n"),
    io:format(" 2. Earth's moon ~n"),
    io:format(" 3. Mars ~n"),
    Answer = io:get_line("Which? > "),
    Value = hd(Answer), % get the first position.
    char_to_planemo(Value).

char_to_planemo(Char) ->
    io:format("~n[chat_to_planemo]~w ~n",[Char]),
    if
	[Char] == "1" -> earth;
	Char == $2 -> moon;
	Char == 51 -> mars % code for "3" is 51
    end.

get_distance() ->
    Input = io:get_line("How far? (meters) >"),
    Value = string:strip(Input, right, $\n), % strip newlines out of user response
    {Distance, _} = string:to_integer(Value),
    io:format("[get_distance] Distance:[~w] ~n", [Distance]),
    Distance.

chars() ->
    io:format("Which planemo are you on?~n"),
    io:format(" 1. Earth ~n"),
    io:format(" 2. Earth's moon ~n"),
    io:format(" 3. Mars ~n"),
    io:get_chars("Which? > ",1). % just get the first character

term() ->
    Input = io:read("what {planemo, distance}? >>"),
    process_term(Input).

process_term({ok, Term}) when is_tuple(Term) ->
    Velocity = drop:fall_velocity(Term),
    io:format("Yields ~w. ~n", [Velocity]),
    term();

process_term({ok, quit}) ->
    io:format("Goodbye. ~n");
    % does not call term() again

process_term({ok, _}) ->
    io:format("U must enter a tuple. ~n"),
    term();

process_term({error, _}) ->
    io:format("U must enter a tuple with correct syntax.~n"),
    term().



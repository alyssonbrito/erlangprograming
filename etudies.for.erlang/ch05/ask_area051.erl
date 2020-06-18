%% @doc Solution to Étude 5.1 Validating Input
%% @author: Alysson Brito <alyssonbrito@gmail.com>
%% @copyright 2020
%% @reference From the Book: Études for Erlang. J. David. Eisenberg
%% @version 0.1

-module(ask_area051).
-export([area/0]).

%% @doc Read inputs form user
%% and calculates the area of given rectangle
%% given the height and width. Returns calculated area
-spec(area() -> number()).

area()->
    dialog().

dialog() ->
    % get the shape
    MenuOption = get_char("R)ectangle, T)riangle, or E)llipse"),
    Shape = char_to_shape(MenuOption),
    % get the width, height
    {W,H} = get_dimensions("Enter Width","Enter Hight"),
    % calculate area
    calculate(Shape,W,H).


% Helper functions
get_char(Message) ->
    Prompt = Message ++ " > ",
    Input = io:get_line(Prompt),         %io:get_chars(Prompt,1),
    hd(Input).

char_to_shape(Char) ->
    if 
        (Char == $T) orelse (Char == $t) -> triangle;
        [Char] == "R" orelse [Char] == "r" -> rectangle;
        Char == $E orelse Char == $e -> ellipse;
        true -> unknown
    end.

get_dimensions(StringA,StringB) ->
    N1 = get_number(StringA),
    N2 = get_number(StringB),
    {N1,N2}.

get_number(Message) ->
    Prompt = Message ++ " > ",
    Input = io:get_line(Prompt),
    to_numeric(Input).

%% @doc Calculates the area of given rectangle
%% given the height and width. Returns calculated area
%% @param atom Either rectangle, triangle or ellipse
%% @param Shape
%% @param N1 Width
%% @param N2 height
-spec(area(atom(),number(),number()) -> number()).

calculate(unknown,_,_) ->
    showErrorMessage("Shape is invalid!"),
    error;
calculate(Shape,N1,N2) ->
    ValidParams =  is_numeric(N1) and is_numeric(N2),
    Result = case ValidParams of
        true -> geom041:area(Shape,N1,N2);
        false -> 
            showErrorMessage("Invalid dimension!"),
            error
    end,
    Result.

% -------------------------------- Utils
is_numeric(N) ->
    is_integer(N) orelse is_float(N).

to_numeric(N) ->
    Number = case string:to_float(N) of
        {error,_} -> 
            case string:to_integer(N) of
                {error,_} -> error;
                {Num,_Decimal} -> Num
            end;
        {Num,[]} -> Num
    end,
    Number.


showErrorMessage(ErrorMessage) ->
    io:format("~s~n",[ErrorMessage]).

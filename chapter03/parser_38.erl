% author: alyssonbrito@gmail.com
% 2019.Jul
% Solution to exercises in Erlang Programming (2009), by Francesco Cesarini and Simon Thompson
% Exercise 3.8

-module(parser_38).
-export([parser/1]).

parser(S) ->
    Representation = m_parser(S);

m_parser("",_Exp,_Resul) -> "";
m_parser(" ",_Exp,_Resul) -> "";
m_parser(" ") -> "";
m_parser(K) when is_integer(K)-> {num,K};
    m_parser(K) when K == "+"-> {,K}; % equal
m_parser(Expr) ->
    T = sub_string(Exp,1,1),
    case T of
	is_number(T) ->
	is_operator(T) -> add_to_result(operator,)







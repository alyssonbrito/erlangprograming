-module(etf).
-export([encoding/0, decoding/0]).

-spec encoding() -> iolist().
encoding() ->
  % _MyTerm = [{key, "value"},1,2,3],
  MyTerm = [{person,[{address,"street"},{name,"Name"},{work,{prof,"prof"}}]},1,2,3],
  io:format("term: ~p~n", [MyTerm]),
  MyETF = erlang:term_to_binary(MyTerm),
  io:format("encoded: ~p~n", [MyETF]),
  MyETF.

-spec decoding() -> term().
decoding() ->
  MyBinary = encoding(),
  MyTerm = erlang:binary_to_term(MyBinary),
  io:format("decoded: ~p~n", [MyTerm]),
  MyTerm.

-module(tail).
-export([tail_len/1]).

tail_len([]) -> 0;
%tail_len([ _H | [] ]) -> 0;
tail_len([ _H| T ]) -> tail_len(T) + 1.


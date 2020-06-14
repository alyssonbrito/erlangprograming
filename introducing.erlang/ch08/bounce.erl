-module(bounce).
-export([report/1]).

% report() ->
%     receive
% 	X -> io:format(" Received ~p ~n",[X])
%     end.

% report() ->
%     receive
% 	X -> io:format("Received ~p ~n",[X]),
% 	     report()
%     end.

% report(Counter) ->
%     receive
%  	X -> io:format("Received Counter:~p Times:~p ~n",[Counter,X]),
%  	     report(Counter+1)
%     end.

report(Counter) ->
    NewCounter = receive
 	X -> io:format("Received Counter:~p Times:~p ~n",[Counter,X]),
 	     Counter+1
    end,
    report(NewCounter).





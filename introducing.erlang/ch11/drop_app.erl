%%% Application interface
%%% Just starts the supervisor


-module(drop_app).
-behavior(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    drop_sup:start_link().

stop(_State) ->
    ok.


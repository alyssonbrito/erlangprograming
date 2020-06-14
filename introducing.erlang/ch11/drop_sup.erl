%%% Supervisor interface
%%% Basically 2 maps into init return

-module(drop_sup).
-behavior(supervisor).
-export([start_link/0]). % convience call for startup
-export([init/1]). % supervisor callback
-define(SERVER, ?MODULE).

%%% convenience method for startup
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%% supervisor callback
init([]) ->
    SuperFlags = #{strategy => one_for_one,
		  intensity => 1,
		  period => 5},

    Drop = #{id => 'drop',
	    start => {'drop', start_link, []},
	    restart => permanent,
	    shutdown => 5000,
	    type => worker,
	    modules => ['drop']},
    {ok, {SuperFlags, [Drop]}}.



-module(damocles_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  damocles_lib:teardown_traffic_control(),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [{damocles, {damocles, start_link, []}, transient, 5000, worker, [damocles_server]}],
	{ok, {{one_for_one, 1, 5}, Procs}}.

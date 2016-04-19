-module(vtrack_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [
    {
      sample_track,
      {sample_track, start_link, ["RIH0.txt"]},
      permanent,
      1,
      worker,
      [sample_track]
    }
  ],
  {ok, {{one_for_one, 1, 5}, Procs}}.

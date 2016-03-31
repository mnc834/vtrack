-module(vtrack_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [{"/test", cowboy_static, {priv_file, vtrack, "test.html"}}]}
  ]),
  cowboy:start_http(my_http_listener, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ),
  vtrack_sup:start_link().

stop(_State) ->
  ok.

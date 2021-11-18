-module(vtrack_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_',
      [
        %%static html pages
        {"/test", cowboy_static, {priv_file, vtrack, "test.html"}},
        {"/lss_test", cowboy_static, {priv_file, vtrack, "lss_test.html"}},
        {"/sample_test", cowboy_static, {priv_file, vtrack, "sample_test.html"}},

        %%js files
        {"/vjs/[...]", cowboy_static, {priv_dir, vtrack, "vjs/src"}},

        %%polynomial coefficients calculator
        {"/lss", lss_handler, []},

        %%working with samples
        {"/sample/read_catalog", sample_handler, [read_catalog]},
        {"/sample/open_file", sample_handler, [open_file]},
        {"/sample/next_chunk", sample_handler, [next_chunk]},
        {"/sample/indicators", sample_handler, [indicators]},
        {"/sample/test_interval", sample_handler, [test_interval]},
        {"/sample/evaluate_interval", sample_handler, [evaluate_interval]}
      ]
    }
  ]),
  {ok, _Pid} = cowboy:start_http(my_http_listener, 100, [{port, 8080}],
                [{env, [{dispatch, Dispatch}]}]
               ),
  vtrack_sup:start_link().

stop(_State) ->
  ok.

-module(sample_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
}).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  Resp_hdr =
    [
      {<<"content-type">>, <<"text/plain">>}
    ],

  {ok, Data} = sample_track:get_js_encoded_data(),
  Resp_body = Data,

  {ok, Req_resp} = cowboy_req:reply(200, Resp_hdr, Resp_body, Req),
  {ok, Req_resp, State}.

terminate(_Reason, _Req, _State) ->
  ok.

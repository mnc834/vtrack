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

  Resp =
    case sample_track:read_next_chunk() of
      {ok, Data} ->
        #{status => ok, ticks => Data};
      eof ->
        #{status => eof};
      {error, Reason} ->
        #{status => error, error => Reason}
    end,
  Resp_body = jsx:encode(Resp),

  {ok, Req_resp} = cowboy_req:reply(200, Resp_hdr, Resp_body, Req),
  {ok, Req_resp, State}.

terminate(_Reason, _Req, _State) ->
  ok.

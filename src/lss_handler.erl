-module(lss_handler).
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

  {Query_bin, Req_1} = cowboy_req:qs_val(<<"query">>, Req),
  Query = jsx:decode(Query_bin, [{labels, atom}, return_maps]),
  #{from := From, to := To, n_points := N, coefficients := Coefs} = Query,

  Power = length(Coefs) - 1,
  X_delta = (To - From) / (N - 1),
  Points =
    [
      begin
        X = From + I * X_delta,
        {X, tapol_epol:calc_val(Coefs, X)}
      end || I <- lists:seq(0, N - 1)
    ],

  Body =
    case tapol_lss:get_least_squares_solution(Points, Power) of
      {ok, Polynomial} ->
        #{status => ok, coefficients => Polynomial};
      {error, Error} ->
        #{status => error, reason => Error}
    end,

  Resp_body = jsx:encode(Body),

  {ok, Req_resp} = cowboy_req:reply(200, Resp_hdr, Resp_body, Req_1),
  {ok, Req_resp, State}.

terminate(_Reason, _Req, _State) ->
  ok.


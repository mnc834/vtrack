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
        {X, calc_polinomial_v(Coefs, X)}
      end || I <- lists:seq(0, N - 1)
    ],

  Body =
    case lss:get_least_squares_solution(Points, Power) of
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec calc_polinomial_v([float()], float()) -> float().
%% @doc calculates a value of a polynomial in a given point,
%%      polynomial coefficients are in order when the coefficients with
%%      the greatest power comes forst
calc_polinomial_v([H | T], X) ->
  F =
    fun(C, Acc) ->
      Acc * X + C
    end,
  lists:foldl(F, H, T).

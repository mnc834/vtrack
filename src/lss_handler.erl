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

  X_delta = (To - From) / (N - 1),
  Points =
    [
      begin
        X = From + I * X_delta,
        #{x => X, y => calc_polinomial_v(Coefs, X)}
      end || I <- lists:seq(0, N - 1)
    ],
  Points_out = jsx:encode(Points),

  {ok, Req_resp} = cowboy_req:reply(200, Resp_hdr, Points_out, Req_1),
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

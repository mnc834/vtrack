-module(sample_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
  operation :: next_chunk | indicators
}).

init(_, Req, [next_chunk]) ->
  {ok, Req, #state{operation = next_chunk}};
init(_Type, Req, [indicators]) ->
  {ok, Req, #state{operation = indicators}}.

handle(Req, State=#state{operation = next_chunk}) ->
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
  {ok, Req_resp, State};
handle(Req, State=#state{operation = indicators}) ->
  Resp_hdr =
    [
      {<<"content-type">>, <<"text/plain">>}
    ],

  {Query_bin, Req_1} = cowboy_req:qs_val(<<"params">>, Req),
  Query = jsx:decode(Query_bin, [{labels, atom}, return_maps]),
  #{x_shift := X_shift, y_shift := Y_shift, polynomials := Polynomials} = Query,

  Map_fun =
    fun(#{left := From, right := To, power := Power}) ->
      L = sample_track:slice_ticks(From, To),
      V = [{T + X_shift, P + Y_shift} || #{price := P, time := T} <- L],
      case lss:get_least_squares_solution(V, Power) of
        {ok, Coefs} ->
          #{status => ok, coefficients => Coefs};
        {error, Reason} ->
          #{status => error, error => Reason}
      end
    end,
  Resp = lists:map(Map_fun, Polynomials),
  Resp_body = jsx:encode(Resp),

  {ok, Req_resp} = cowboy_req:reply(200, Resp_hdr, Resp_body, Req_1),
  {ok, Req_resp, State}.

terminate(_Reason, _Req, _State) ->
  ok.

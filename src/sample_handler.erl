-module(sample_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
  operation :: next_chunk | indicators | test_interval
}).

init(_, Req, [next_chunk]) ->
  {ok, Req, #state{operation = next_chunk}};
init(_Type, Req, [indicators]) ->
  {ok, Req, #state{operation = indicators}};
init(_Type, Req, [test_interval]) ->
  {ok, Req, #state{operation = test_interval}}.

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
      case tapol_lss:get_least_squares_solution(V, Power) of
        {ok, Coefs} ->
          #{status => ok, coefficients => Coefs};
        {error, Reason} ->
          #{status => error, error => Reason}
      end
    end,
  Resp = lists:map(Map_fun, Polynomials),
  Resp_body = jsx:encode(Resp),

  {ok, Req_resp} = cowboy_req:reply(200, Resp_hdr, Resp_body, Req_1),
  {ok, Req_resp, State};
handle(Req, State=#state{operation = test_interval}) ->
  Resp_hdr =
    [
      {<<"content-type">>, <<"text/plain">>}
    ],

  {Query_bin, Req_1} = cowboy_req:qs_val(<<"params">>, Req),
  Query = jsx:decode(Query_bin, [{labels, atom}, return_maps]),
  #{
    x_shift := X_shift,
    y_shift := Y_shift,
    left := Left,
    right := Right,
    mean_span := Mean_span,
    mean_power := Mean_power,
    short_span := Short_span,
    short_power := Short_power
  } = Query,

  Gen_left_predicate =
    fun(T) ->
      fun({X, _Y}) when X < T ->
        true;
        (_) ->
          false
      end
    end,

  L = [{T + X_shift, P + Y_shift} || #{price := P, time := T} <- sample_track:slice_ticks(Left - Mean_span, Left)],
  Interval = sample_track:slice_ticks(Left, Right),
  Foldl_fun =
    fun(#{price := P, time := T} = Tick, #{short_v := Short_v, long_v := Long_v, list := List} = Acc) ->
      X = T + X_shift,
      Y = P + Y_shift,
      Mean_left = X - Mean_span,
      Short_left = X - Short_span,
      Mean_list = lists:dropwhile(Gen_left_predicate(Mean_left), List ++ [{X, Y}]),
      Short_list = lists:dropwhile(Gen_left_predicate(Short_left), Mean_list),
      Mean_result = tapol_lss:get_least_squares_solution(Mean_list, Mean_power),
      Short_result = tapol_lss:get_least_squares_solution(Short_list, Short_power),
      New_acc = Acc#{list => Mean_list},
      case {Mean_result, Short_result} of
        {{ok, Mean_p}, {ok, Short_p}} ->
          Mean_val = tapol_epol:calc_val(Mean_p, X),
          D_mean_p = tapol_epol:derivative(Mean_p),
          D_mean_val = tapol_epol:calc_val(D_mean_p, X),
          D_short_p = tapol_epol:derivative(Short_p),
          D_short_val = tapol_epol:calc_val(D_short_p, X),
          Short_val = tapol_epol:calc_val(Short_p, X),
          case D_mean_val > 0 of
            true ->
              %%possible long
              case (Short_val < Mean_val) andalso (D_short_val > 0) of
                true ->
                  %%long
                  New_acc#{long_v => Long_v ++ [Tick]};
                false ->
                  New_acc
              end;
            false ->
              %%possible short
              case (Short_val > Mean_val) andalso (D_short_val < 0) of
                true ->
                  %%short
                  New_acc#{short_v => Short_v ++ [Tick]};
                false ->
                  New_acc
              end
          end;
        _ ->
          New_acc
      end
    end,
  #{short_v := Short_v, long_v := Long_v} =
    lists:foldl(Foldl_fun, #{short_v => [], long_v => [], list => L}, Interval),

  Resp = #{status => ok, short_list => Short_v, long_list => Long_v},
  Resp_body = jsx:encode(Resp),

  {ok, Req_resp} = cowboy_req:reply(200, Resp_hdr, Resp_body, Req_1),
  {ok, Req_resp, State}.

terminate(_Reason, _Req, _State) ->
  ok.

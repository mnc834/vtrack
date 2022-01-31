-module(sample_handler).
-behaviour(cowboy_http_handler).

-define(CATALOG_NAME, "CATALOG.txt").
-define(RESPONSE_HDRS, [{<<"content-type">>, <<"text/plain">>}]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
  operation :: read_catalog | open_file | next_chunk | indicators | test_interval | evaluate_interval
}).

init(_, Req, [read_catalog]) ->
  {ok, Req, #state{operation = read_catalog}};
init(_, Req, [open_file]) ->
  {ok, Req, #state{operation = open_file}};
init(_, Req, [next_chunk]) ->
  {ok, Req, #state{operation = next_chunk}};
init(_Type, Req, [indicators]) ->
  {ok, Req, #state{operation = indicators}};
init(_Type, Req, [test_interval]) ->
  {ok, Req, #state{operation = test_interval}};
init(_Type, Req, [evaluate_interval]) ->
  {ok, Req, #state{operation = evaluate_interval}}.

handle(Req, State=#state{operation = read_catalog}) ->
  Resp =
    case sample_track:read_catalog(?CATALOG_NAME) of
      {ok, Catalog} ->
        %% only names of sequrities are sent in the response
        Resp_catalog = lists:map(fun(#{name := Name}) -> #{name => list_to_binary(Name)} end, Catalog),
        #{status => ok, catalog => Resp_catalog};
      {error, Reason} ->
        #{status => error, error => Reason}
    end,
  Resp_body = jsx:encode(Resp),

  {ok, Req_resp} = cowboy_req:reply(200, ?RESPONSE_HDRS, Resp_body, Req),
  {ok, Req_resp, State};

handle(Req, State=#state{operation = open_file}) ->
  {Name_bin, Req_1} = cowboy_req:qs_val(<<"symbol">>, Req),

  Resp =
    case sample_track:open_file(binary_to_list(Name_bin)) of
      {ok, Type} ->
        #{status => ok, type => Type};
      {error, Reason} ->
        #{status => error, error => Reason}
    end,
  Resp_body = jsx:encode(Resp),

  {ok, Req_resp} = cowboy_req:reply(200, ?RESPONSE_HDRS, Resp_body, Req_1),
  {ok, Req_resp, State};
handle(Req, State=#state{operation = next_chunk}) ->
  Resp =
    case sample_track:read_next_chunk() of
      {ok, Data} ->
        #{status => ok, data => Data};
      eof ->
        #{status => eof};
      {error, Reason} ->
        #{status => error, error => Reason}
    end,
  Resp_body = jsx:encode(Resp),

  {ok, Req_resp} = cowboy_req:reply(200, ?RESPONSE_HDRS, Resp_body, Req),
  {ok, Req_resp, State};
handle(Req, State=#state{operation = indicators}) ->
  {Query_bin, Req_1} = cowboy_req:qs_val(<<"params">>, Req),
  Query = jsx:decode(Query_bin, [{labels, atom}, return_maps]),
  #{x_shift := X_shift, y_shift := Y_shift, polynomials := Polynomials} = Query,

  Map_fun =
    fun(#{left := From, right := To, power := Power}) ->
      L = trals:convert_to_ticks(sample_track:slice_data(From, To)),
      V = [{T + X_shift, P + Y_shift} || #{price := P, time := T} <- L],
      case tapol_lss:get_least_squares_solution(V, Power) of
        {ok, Coefs} ->
          #{status => ok, coefficients => Coefs, sigma => calc_sigma(Coefs, V)};
        {error, Reason} ->
          #{status => error, error => Reason}
      end
    end,
  Resp = lists:map(Map_fun, Polynomials),
  Resp_body = jsx:encode(Resp),

  {ok, Req_resp} = cowboy_req:reply(200, ?RESPONSE_HDRS, Resp_body, Req_1),
  {ok, Req_resp, State};
handle(Req, State=#state{operation = test_interval}) ->
  {Query_bin, Req_1} = cowboy_req:qs_val(<<"params">>, Req),
  Query = jsx:decode(Query_bin, [{labels, atom}, return_maps]),
  #{
    left := Left,
    right := Right,
    mean_span := Mean_span,
    mean_power := Mean_power,
    short_span := Short_span,
    short_power := Short_power,
    absolute_diff := Absolute_diff
  } = Query,

  Two_curves_param = #{mean_span => Mean_span,
                       mean_power => Mean_power,
                       short_span => Short_span,
                       short_power => Short_power,
                       absolute_diff => Absolute_diff},
  {ok, Alg_state} = trals:two_curves_init(Two_curves_param, Left),

  Interval = sample_track:slice_data(Left, Right),
  Foldl_fun =
    fun(Item, #{short_v := Short_v, long_v := Long_v, state := S} = Acc) ->
      {ok, Result, New_state} = trals:two_curves_process(Item, S),
      New_acc = Acc#{state => New_state},
      case Result of
        {long, Tick} ->
          New_acc#{long_v => Long_v ++ [Tick]};
        {short, Tick} ->
          New_acc#{short_v => Short_v ++ [Tick]};
        none ->
          New_acc
      end
    end,

  #{short_v := Short_v, long_v := Long_v} =
    lists:foldl(Foldl_fun, #{short_v => [], long_v => [], state => Alg_state}, Interval),

  Resp = #{status => ok, short_list => Short_v, long_list => Long_v},
  Resp_body = jsx:encode(Resp),

  {ok, Req_resp} = cowboy_req:reply(200, ?RESPONSE_HDRS, Resp_body, Req_1),
  {ok, Req_resp, State};

handle(Req, State=#state{operation = evaluate_interval}) ->
  {Query_bin, Req_1} = cowboy_req:qs_val(<<"params">>, Req),
  Query = jsx:decode(Query_bin, [{labels, atom}, return_maps]),
  #{
    x_shift := X_shift,
    y_shift := Y_shift,
    left := Left,
    right := Right,
    max_p := Max_p,
    max_l := Max_l
  } = Query,

  Gen_pos =
    fun(#{price := P, time := T}, Direction) ->
      fun Pos_fun(#{price := P_cur, time := T_cur},
          #{l_p_list := L_p_list, l_l_list := L_l_list,
            s_p_list := S_p_list, s_l_list := S_l_list, fun_list := Fun_list} = Acc) ->
        Diff =
          case Direction of
            long ->
              P_cur - P;
            short ->
              P - P_cur
          end,
        Profitable =
          case (Diff > Max_p) of
            true ->
              %%profitable deal
              true;
            false ->
              case (-1 * Diff) > Max_l of
                true ->
                  %%unprofitable deal
                  false;
                false ->
                  undefined
              end
          end,
        case is_boolean(Profitable) of
          true ->
            Deal = #{
              price => P,
              time => T,
              profitable => Profitable,
              close_price => P_cur,
              close_time => T_cur
            },
            case {Direction, Profitable} of
              {long, true} ->
                %% add the deal to the long profit list
                Acc#{l_p_list => L_p_list ++ [Deal]};
              {long, false} ->
                %% add the deal to the long losses list
                Acc#{l_l_list => L_l_list ++ [Deal]};
              {short, true} ->
                %% add the deal to the short profit list
                Acc#{s_p_list => S_p_list ++ [Deal]};
              {short, false} ->
                %% add the deal to the short losses list
                Acc#{s_l_list => S_l_list ++ [Deal]}
            end;
          false ->
            %% keep the position on the long list
            Acc#{fun_list => Fun_list ++ [Pos_fun]}
        end
      end
    end,

  Shift_right = Right + X_shift,

  Foldl_fun =
    fun(#{time := T} = Tick, #{fun_list := Fun_list} = Acc) ->
      #{fun_list := New_fun_list} = New_acc =
        lists:foldl(fun(Fun, Cur_acc) -> Fun(Tick, Cur_acc) end,
          Acc#{fun_list => []}, Fun_list),
      case T < Shift_right of
        true ->
          %%have to open a position
          Long = Gen_pos(Tick, long),
          Short = Gen_pos(Tick, short),
          %%and add the to acc
          {true, New_acc#{fun_list => New_fun_list ++ [Long, Short]}};
        false ->
          %% have to check if the processing should be stopped
          {New_fun_list =/= [], New_acc}
      end
    end,


  L = [#{price => P + Y_shift, time => T + X_shift} ||
    #{price := P, time := T} <- sample_track:slice_data_after(Left)],

  Init_state = #{
    l_p_list => [],
    l_l_list => [],
    s_p_list => [],
    s_l_list => [],
    fun_list => []
  },

  #{l_p_list := L_p_list, l_l_list := L_l_list,
    s_p_list := S_p_list, s_l_list := S_l_list} =
    foldl_while(Foldl_fun, Init_state, L),



  Resp = #{
    status => ok,
    l_p_list => L_p_list,
    l_l_list => L_l_list,
    s_p_list => S_p_list,
    s_l_list => S_l_list
  },

  Process_fun =
    fun(#{price := P, time := T, profitable := Profitable, close_price := C_p, close_time := C_t},
        #{count := N, sum := S, time := V}) ->
      Diff = abs(C_p - P),
      Add =
        case Profitable of
          true ->
             Diff;
          false ->
            -1 * Diff
        end,
      #{count => N + 1, sum => S + Add, time => V + C_t - T}
    end,

  For_each_fun =
    fun({Name, Deals}) ->
      #{count := N, sum := S, time := T} =
        lists:foldl(Process_fun, #{count => 0, sum => 0, time => 0}, Deals),
      {Average_time, Average_p} =
        case N > 0 of
          true ->
            {
              calendar:seconds_to_daystime(round(T / (N * 1000))),
              S / N
            };
          false ->
            {"N/A", "N/A"}
        end,
      io:format("~p: N = ~p; sum = ~.3f;(~.3f); mean_time = ~p~n", [Name, N, S, Average_p, Average_time])
    end,

  io:format("~n========== max_p = ~p; max_l = ~p =============~n", [Max_p, Max_l]),
  lists:foreach(For_each_fun, [{l_p, L_p_list}, {l_l, L_l_list}, {s_p, S_p_list}, {s_l, S_l_list}]),
  io:format("================================================~n"),

  Resp_body = jsx:encode(Resp),

  {ok, Req_resp} = cowboy_req:reply(200, ?RESPONSE_HDRS, Resp_body, Req_1),
  {ok, Req_resp, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec foldl_while(Fun, Acc0, List) -> Acc1 when
  Fun :: fun((Elem :: T, Acc_in) -> {boolean(), Acc_out}),
  Acc0 :: term(),
  Acc1 :: term(),
  Acc_in :: term(),
  Acc_out :: term(),
  List :: [T],
  T :: term().
%% @doc implements foldl operation on the list while Fun returns {true, _} or end of list is reached,
%%      if Fun returns {false, Acc_out} -> Acc_out is returned and processing is stopped
foldl_while(_Fun, Acc_in, []) ->
  Acc_in;
foldl_while(Fun, Acc_in, [H | T]) ->
  {Continue, Acc_out} = Fun(H, Acc_in),
  case Continue of
    true ->
      foldl_while(Fun, Acc_out, T);
    false ->
      Acc_out
  end.

-spec calc_sigma(tapol_epol:e_polynomial(), trals:ticks()) -> float().
%% @doc calculates sigma for the given set of ticks and approximation
calc_sigma(_P, []) ->
  0.0;
calc_sigma(P, L) ->
  Fun = fun({X, Y}, {Sum, N}) ->
          Diff = Y - tapol_epol:calc_val(P, X),
          {Sum + Diff * Diff, N + 1}
        end,
  {Sum, N} = lists:foldl(Fun, {0, 0}, L),
  math:sqrt(Sum / N).

%%%===================================================================
%%% Unit tests
%%%===================================================================
-ifdef(TEST).

foldl_while_test_() ->
  N = 10,
  L = lists:seq(1, N),

  Gen_count =
    fun(S) ->
      fun(X, Acc) ->
        {X < S, Acc + 1}
      end
    end,

  Res_1 = foldl_while(Gen_count(N + 1), 0, []),
  Res_2 = foldl_while(Gen_count(N + 1), 0, L),
  Res_3 = foldl_while(Gen_count(N), 0, L),
  Res_4 = foldl_while(Gen_count(5), 0, L),

  [
    ?_assertEqual(0, Res_1),
    ?_assertEqual(N, Res_2),
    ?_assertEqual(N, Res_3),
    ?_assertEqual(5, Res_4)
  ].

-endif.
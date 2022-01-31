%%%-------------------------------------------------------------------
%%% @author MNC834
%%% @copyright (C) 2022, <COMPANY>
%%% @doc TRading ALgorithmS (TRALS)
%%%
%%% @end
%%% Created : 27. Jan 2022 11:54 AM
%%%-------------------------------------------------------------------
-module(trals).
-author("MNC834").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([
  convert_to_ticks/1,
  two_curves_init/2,
  two_curves_process/2
]).

-export_type([
  tick/0,
  ticks/0,
  param/0,
  init_fun/0,
  process_fun/0]).

-type tick() :: #{time := times:time(), price := float()}.
%% @doc denotes a single price/time point

-type ticks() :: [tick()].
%% @doc list of ticks

-type param() :: #{atom() := term()}.
%% @doc an algorithm's parameters

-type state() :: term().
%% @doc an algorithm's state

-type trade() :: trades_processor:trade().
-type candle() :: candle_processor:candle().
-type data_item() :: trade() | candle().
-type data() :: sample_track:data().

-type init_fun() :: fun((param(), Start_time :: times:time()) -> {ok, state()} | {error, Reason :: term()}).
%% @doc a function that initiates an algorithm using algorithm's specific parameters and
%% the start cutoff time

-type process_fun() :: fun((data_item(), state()) -> {ok, none | {long | short, tick()}, state()} |
                                                     {error, Reason :: term()}).
%% @doc an algorithm function that processes the next piece of data and returns a result of assessment

-type two_curves_param() :: #{mean_span := times:time(),
                              mean_power := integer(),
                              short_span := times:time(),
                              short_power := integer(),
                              absolute_diff := float()}.

-record(two_curves_state,{
  x_shift :: times:time(),
  y_shift :: float(),
  mean_span :: times:time(),
  mean_power :: integer(),
  short_span :: times:time(),
  short_power :: integer(),
  absolute_diff :: float(),
  data_type :: catalog_processor:data_type(),
  points :: [{times:time(), float()}]
}).

-spec convert_to_ticks(data()) -> ticks().
%% @doc converts trades or candles into ticks
convert_to_ticks(Data) ->
  Foldr_fun =
    fun(Item, L) ->
      Ticks = convert_item_to_ticks(Item),
      lists:append(Ticks, L)
    end,
  lists:foldr(Foldr_fun, [], Data).


-spec two_curves_init(two_curves_param(), Start_time :: times:time()) -> {ok, #two_curves_state{} } | {error, Reason :: term()}.
%% @doc initiates two curves algorithm
two_curves_init(Param, Start_time) ->
  case Param of
    #{mean_span := Mean_span,
      mean_power := Mean_power,
      short_span := Short_span,
      short_power := Short_power,
      absolute_diff := Absolute_diff
    } ->
      Data = sample_track:slice_data(Start_time - Mean_span, Start_time),
      case determine_data_type(Data) of
        {ok, Data_type} ->
          Ticks = convert_to_ticks(Data),
          [#{time := T0, price := P0} | _] = Ticks,
          X_shift = -1 * T0,
          Y_shift = -1 * P0,
          Points = [{T +  X_shift, P + Y_shift} || #{time := T, price := P} <- Ticks],
          State = #two_curves_state{
            x_shift = X_shift,
            y_shift = Y_shift,
            mean_span = Mean_span,
            mean_power = Mean_power,
            short_span = Short_span,
            short_power = Short_power,
            absolute_diff = Absolute_diff,
            data_type = Data_type,
            points = Points},
          {ok, State};
        {error, Reason} ->
          {error, Reason}
      end;
    _ ->
      {error, invalid_two_curves_parameter}
  end.

-spec two_curves_process(data_item(), state()) -> {ok, none | {long | short, tick()}, state()} |
                                                  {error, Reason :: term()}.
%% @doc two curves algorithm processing function
two_curves_process(Item, #two_curves_state{x_shift = X_shift, y_shift = Y_shift, points = Points,
  mean_span = Mean_span, short_span = Short_span, mean_power = Mean_power, short_power = Short_power,
  absolute_diff = Absolute_diff} = State) ->
  Gen_left_predicate =
    fun(T) ->
      fun({X, _Y}) when X < T ->
        true;
        (_) ->
          false
      end
    end,

  Buy_order =
    fun(V) ->
      Level = V - Y_shift - Absolute_diff,
      case Item of
        #{price := P, time := T} ->
          %% trade
          case P < Level of
            true ->
              {true, #{price => P, time => T}};
            false ->
              {false, 0}
          end;
        #{day_low := Low, day_high := High, open_price := Open, close_time := T} ->
          %% candle
          case Low < Level of
            true ->
              P = case High > Level of
                    true ->
                      Level;
                    false ->
                      Open
                  end,
              {true, #{price => P, time => T}};
            false ->
              {false, 0}
          end
      end
    end,

  Sell_order =
    fun(V) ->
      Level = V - Y_shift + Absolute_diff,
      case Item of
        #{price := P, time := T} ->
          %% trade
          case P > Level of
            true ->
              {true, #{price => P, time => T}};
            false ->
              {false, 0}
          end;
        #{day_high := High, day_low := Low, open_price := Open, close_time := T} ->
          %% candle
          case High > Level of
            true ->
              P = case Low < Level of
                    true ->
                      Level;
                    false ->
                      Open
                  end,
              {true, #{price => P, time => T}};
            false ->
              {false, 0}
          end
      end
    end,

  Ticks = convert_item_to_ticks(Item),
  [{X, _Y} = First_point| Tail] = [{T +  X_shift, P + Y_shift} || #{time := T, price := P} <- Ticks],
  Mean_left = X - Mean_span,
  Short_left = X - Short_span,
  Mean_list = lists:dropwhile(Gen_left_predicate(Mean_left), Points ++ [First_point]),
  Short_list = lists:dropwhile(Gen_left_predicate(Short_left), Mean_list),
  Mean_result = tapol_lss:get_least_squares_solution(Mean_list, Mean_power),
  Short_result = tapol_lss:get_least_squares_solution(Short_list, Short_power),
  New_state = State#two_curves_state{points = Mean_list ++ Tail},
  case {Mean_result, Short_result} of
    {{ok, Mean_p}, {ok, Short_p}} ->
      Mean_val = tapol_epol:calc_val(Mean_p, X),
      D_mean_p = tapol_epol:derivative(Mean_p),
      D_mean_val = tapol_epol:calc_val(D_mean_p, X),
      D_short_p = tapol_epol:derivative(Short_p),
      D_short_val = tapol_epol:calc_val(D_short_p, X),
      Result =
        case D_mean_val > 0 of
          true ->
            %%possible long
            {Buy, Buy_tick} = Buy_order(Mean_val),
            case (Buy) andalso (D_short_val > 0) andalso (D_short_val > D_mean_val) of
              true ->
                %%long
                {long, Buy_tick};
              false ->
                none
            end;
          false ->
            %%possible short
            {Sell, Sell_tick} = Sell_order(Mean_val),
            case (Sell) andalso (D_short_val < 0) andalso (D_short_val < D_mean_val) of
              true ->
                %%short
                {short, Sell_tick};
              false ->
                none
            end
        end,
      {ok, Result, New_state};
    _ ->
      {ok, none, New_state}
  end.


%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec determine_data_type(data()) -> {ok, catalog_processor:data_type()} | {error, Reason :: term()}.
%% @doc determines the type of data (trades or candles)
determine_data_type([]) ->
  {erroe, empty_list};
determine_data_type([#{time := _Time, price := _Price} | _Tail]) ->
  {ok, trades};
determine_data_type([#{close_time := _C_t, close_price := _C_p} | _Tail]) ->
  {ok, candles};
determine_data_type(_) ->
  {error, unknown_data_type}.

-spec convert_item_to_ticks(data_item()) -> ticks().
%% @doc converts a trade or a candle into tick(s)
convert_item_to_ticks(#{time := T, price := P}) ->
  [#{time => T, price => P}];
convert_item_to_ticks(#{open_time := O_t, open_price := O_p, close_time := C_t, close_price := C_p,
                        day_high := High, day_low := Low}) ->
  Dt = round((C_t - O_t) / 10),
  {P1, P2} =
    if
      O_p > C_p ->
        {High, Low};
      true ->
        {Low, High}
      end,
    [
      #{time => O_t, price => O_p},
      #{time => O_t + Dt, price => P1},
      #{time => C_t - Dt, price => P2},
      #{time => C_t, price => C_p}
    ].

-ifdef(TEST).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_to_ticks_test_() ->
  Trades = [#{time => X, price => 100 * X, amount => 100} || X <- lists:seq(1, 10)],
  Trade_ticks = [#{time => T, price => P} || #{time := T, price := P} <- Trades],

  Diff_time = 100,
  Candle_time_span = 10 * Diff_time,
  Candles_1 = [#{open_price => 100 * X,
    day_low => 10 * X,
    day_high => 200 * X,
    close_price => 150 * X,
    volume => 500,
    open_time => 3000 * X,
    close_time => 3000 * X + Candle_time_span} || X <- lists:seq(1, 10)],
  Candle_1_ticks = lists:flatten([[
    #{time => O_t, price => O_p},
    #{time => O_t + Diff_time, price => Low},
    #{time => C_t - Diff_time, price => High},
    #{time => C_t, price => C_p}
  ] ||
    #{open_time := O_t, open_price := O_p, close_time := C_t, close_price := C_p, day_high := High, day_low := Low} <- Candles_1]),

  Candles_2 = [#{open_price => 150 * X,
    day_low => 10 * X,
    day_high => 200 * X,
    close_price => 100 * X,
    volume => 500,
    open_time => 3000 * X,
    close_time => 3000 * X + Candle_time_span} || X <- lists:seq(1, 10)],
  Candle_2_ticks = lists:flatten([[
    #{time => O_t, price => O_p},
    #{time => O_t + Diff_time, price => High},
    #{time => C_t - Diff_time, price => Low},
    #{time => C_t, price => C_p}
  ] ||
    #{open_time := O_t, open_price := O_p, close_time := C_t, close_price := C_p, day_high := High, day_low := Low} <- Candles_2]),

  Res_1 = convert_to_ticks(Trades),
  Res_2 = convert_to_ticks(Candles_1),
  Res_3 = convert_to_ticks(Candles_2),

  [
    ?_assertEqual(Trade_ticks, Res_1),
    ?_assertEqual(Candle_1_ticks, Res_2),
    ?_assertEqual(Candle_2_ticks, Res_3)
  ].

-endif.
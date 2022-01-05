%%%-------------------------------------------------------------------
%%% @author MNC834
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Dec 2021 9:24 PM
%%%-------------------------------------------------------------------
-module(candles_processor).
-author("MNC834").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(OPEN_TIME, {8, 30, 0}).
-define(CLOSE_TIME, {15, 0, 0}).

%% API
-export([candle_map_fun/2]).

-export_type([candle/0, candles/0]).

%% TODO: make time common between trades and candles
-type candle() :: #{open_price := float(),
                    day_low := float(),
                    day_high := float(),
                    close_price := float(),
                    volume := integer(),
                    open_time := times:time(),
                    close_time := times:time()}.
%% @doc a single candle

-type candles() :: [candle()].
%% @doc a list of candles

-spec candle_map_fun(file_reader:file_chunk(), undefined) -> Result
  when Result :: {{true, candle()} | false, Leftover :: file_reader:file_chunk(), undefined}.
%% @doc processes a binary line, extracts a candle information and returns the extracted candle or leftover
%% that can't be processed
candle_map_fun(Chunk, undefined) ->
  case binary:split(Chunk, [<<",">>], [global]) of
    [<<"Date">> | _Taile] ->
      %% header
      {false, <<>>, undefined};
    [Date_str, Open_price_str, Day_low_str, Day_high_str, Close_price_str, Volume_str] ->
      Open_price = binary_to_number(Open_price_str),
      Day_low = binary_to_number(Day_low_str),
      Day_high = binary_to_number(Day_high_str),
      Close_price = binary_to_number(Close_price_str),
      Volume = binary_to_number(Volume_str),
      {ok, Open_time, Close_time} = date_time_str_to_milliseconds(Date_str),
      Candle = #{
        open_price => Open_price,
        day_low => Day_low,
        day_high => Day_high,
        volume => Volume,
        close_price => Close_price,
        open_time => Open_time,
        close_time => Close_time
      },
      {{true, Candle}, <<>>, undefined};
    _ ->
      {false, Chunk, undefined}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec date_time_str_to_milliseconds(Bin_str :: binary()) -> Result
  when Result :: {ok, Open_time :: times:time(), Close_time :: times:time()} | {error, Reason :: term()}.
%% @doc converts string of the following format:
%%      [M]M/[D]D/YYYY into a pair of open and close times
date_time_str_to_milliseconds(Bin_str) ->
  case binary:split(Bin_str, <<"/">>, [global]) of
    [_Month_str, _Day_str, _Year_str] = Str_list ->
      [Month, Day, Year]  = [binary_to_integer(X) || X <- Str_list],
      Open_time = times:to_milliseconds_since_1970({{Year, Month, Day}, ?OPEN_TIME}),
      Close_time = times:to_milliseconds_since_1970({{Year, Month, Day}, ?CLOSE_TIME}),
      {ok, Open_time, Close_time};
    _ ->
      {error, {date_time_str_to_milliseconds, Bin_str}}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec binary_to_number(Bin_str :: binary()) -> float() | integer().
%% @doc converts a binary string into a number
binary_to_number(Bin) ->
  Str = binary_to_list(Bin),
  case string:to_float(Str) of
    {error,no_float} ->
      list_to_integer(Str);
    {F,[]} ->
      F
  end.

-ifdef(TEST).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ticks_map_fun_test_() ->
  P = 100.0,
  P_bin = float_to_binary(P),


  S_prefix = <<"3/16/2020,138,138.4">>,
  S_suffix = <<"7,150.1,", P_bin/bits, ",2416335">>,
  S1 = <<S_prefix/bits, S_suffix/bits>>,

  {false, Res_1, undefined} = candle_map_fun(<<"Date,Open,Day Low,Day High,Close,Volume">>, undefined),
  {{true, #{close_price := P1}}, <<>>, undefined} = candle_map_fun(S1, undefined),
  {false, Res_2, undefined} = candle_map_fun(S_prefix, undefined),

  [
    ?_assertEqual(<<>>, Res_1),
    ?_assertEqual(P, P1),
    ?_assertEqual(S_prefix, Res_2)
  ].

-endif.
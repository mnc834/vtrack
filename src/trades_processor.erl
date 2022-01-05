%%%-------------------------------------------------------------------
%%% @author MNC834
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Nov 2021 1:35 PM
%%%-------------------------------------------------------------------
-module(trades_processor).
-author("MNC834").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([ticks_map_fun/2]).

-export_type([tick/0, ticks/0]).

-type tick() :: #{price := float(), amount := integer(), time := times:time()}.
%% @doc a single trade

-type ticks() :: [tick()].
%% @doc a list of trades

-type last_tick_time() :: undefined | times:time().
%% @doc time of the last trade

-spec ticks_map_fun(file_reader:file_chunk(), last_tick_time()) -> Result
  when Result :: {{true, tick()} | false, Leftover :: file_reader:file_chunk(), last_tick_time()}.
%% @doc processes a binary line, extracts a tick and returns the extracted tick or leftover
%% that can't be processed and last tick's time.
ticks_map_fun(Chunk, Last_tick_time) ->
    case binary:split(Chunk, [<<";">>], [global]) of
      [_Code, _Contract, Price_str, Amount_str, Dat_time_str, _Trade_id, _Nosystem] ->
        Price = binary_to_float(Price_str),
        Amount = binary_to_integer(Amount_str),
        Time = dat_time_str_to_milliseconds(Dat_time_str, Last_tick_time),
        {{true, #{price => Price, amount => Amount, time => Time}}, <<>>, Time};
      _ ->
        {false, Chunk, Last_tick_time}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec dat_time_str_to_milliseconds(Bin_str :: binary(), Prev_time :: times:time() | undefined) -> times:time().
%% @doc 1)converts string of the following format:
%%           YYYY-MM-DD HH:MM:SS<.milisec>
%%        into a number of milliseconds from 1st Jan 1970
%%      2) if Bin_str contains the following format:
%%           DD.MM.YYYY HH:MM
%%         and Prev_time =:= undefined
%%         converts Bin_str to miliseconds from 1st Jan 1970
%%      3) In all other cases
%%      the Prev_time is returned
dat_time_str_to_milliseconds(Bin_str, Prev_time) ->
  Parse_result =
    case binary:split(Bin_str, <<" ">>) of
      [Date_str, Time_str] ->
        case binary:split(Date_str, <<"-">>, [global]) of
          [Year_str, Month_str, Day_str] ->
            %% clause 1
            case binary:split(Time_str, [<<":">>, <<".">>], [global]) of
              [Hour_str, Min_str, Sec_str, Msec_str] ->
                {ok, [Year_str, Month_str, Day_str, Hour_str, Min_str, Sec_str, Msec_str]};
              _ ->
                {error, time_format_is_not_1}
            end;
          _ ->
            case Prev_time =:= undefined of
              true ->
                %%check condition for clause 2
                case binary:split(Date_str, <<".">>, [global]) of
                  [Day_str, Month_str, Year_str] ->
                    case binary:split(Time_str, <<":">>, [global]) of
                      [Hour_str, Min_str] ->
                        {ok, [Year_str, Month_str, Day_str, Hour_str, Min_str, <<"0">>, <<"0">>]};
                      _ ->
                        {error, time_format_is_not_2}
                    end;
                  _ ->
                    {error, date_format_is_not_2}
                end;
              false ->
                {error, date_format_is_not_1_and_prev_time_is_not_undefined}
            end
        end;
      _ ->
        {error, wrong_bin_format}
    end,
  case Parse_result of
    {ok, Str_list} ->
      [Year, Month, Day, Hour, Min, Sec, Msec] =
        [binary_to_integer(X) ||
          X <- Str_list],
      Date_time = {{Year, Month, Day}, {Hour, Min, Sec}},
      times:to_milliseconds_since_1970(Date_time, Msec);
    {error, _Reson} ->
      Prev_time
  end.

-ifdef(TEST).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dat_time_str_to_milliseconds_test_() ->
  T_bin_1 =  <<"2010-01-11 10:30:00.080">>,
  T_bin_2 = <<"11.01.2010 10:30">>,

  Res_1 = dat_time_str_to_milliseconds(T_bin_1, 0),
  Res_2 = dat_time_str_to_milliseconds(T_bin_1, undefined),
  Res_3 = dat_time_str_to_milliseconds(T_bin_2, 0),
  Res_4 = dat_time_str_to_milliseconds(T_bin_2, undefined),

  [
    ?_assertEqual(true, is_integer(Res_1) andalso Res_1 > 0),
    ?_assertEqual(true, is_integer(Res_2) andalso Res_2 > 0),
    ?_assertEqual(true, is_integer(Res_3) andalso Res_3 =:= 0),
    ?_assertEqual(true, is_integer(Res_4) andalso Res_4 > 0)
  ].

ticks_map_fun_test_() ->
  P = 100.0,
  P_bin = float_to_binary(P),

  T_bin = <<"2010-01-11 10:30:00.080">>,
  T = dat_time_str_to_milliseconds(T_bin, 0),

  S_prefix = <<"RIH0;RTS-3.10;">>,
  S_suffix = <<";1;", T_bin/bits, ";129633008;0">>,
  S1 = <<S_prefix/bits, P_bin/bits, S_suffix/bits>>,

  {{true, #{price := P}}, Res_1, T} = ticks_map_fun(<<S1/bits>>, 0),
  {false, Res_2, 0} = ticks_map_fun(S_prefix, 0),
  {{true, #{price := P}}, Res_3, T} = ticks_map_fun(S1, 0),
  {false, Res_4, 0} = ticks_map_fun(<<>>, 0),

  [
    ?_assertEqual(<<>>, Res_1),
    ?_assertEqual(S_prefix, Res_2),
    ?_assertEqual(<<>>, Res_3),
    ?_assertEqual(<<>>, Res_4)
  ].

-endif.



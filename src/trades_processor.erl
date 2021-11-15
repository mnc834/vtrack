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
-export([extract_ticks_from_chunk/2]).

-export_type([tick/0, ticks/0]).

-type tick() :: #{price := float(), amount := integer(), time := integer()}.
%% @doc a single trade

-type ticks() :: [tick()].
%% @doc a list of trades

-type last_tick_time() :: undefined | integer().
%% @doc time of the last trade

-spec extract_ticks_from_chunk(file_reader:file_chunk(), last_tick_time()) -> Result
  when Result :: {ok, ticks(), file_reader:file_chunk(), last_tick_time()}.
%% @doc processes chunk or binary, extracts all the ticks and returns the extracted ticks and
%%      the leftover that can't be processed. Ticks should be separated by "\r\n"
extract_ticks_from_chunk(Chunk, Last_tick_time) ->
  Map_fun =
    fun(Bin_str, {Prev_time, <<>>}) ->
      case binary:split(Bin_str, [<<";">>, <<"\r">>], [global]) of
        [_Code, _Contract, Price_str, Amount_str, Dat_time_str, _Trade_id, _Nosystem, <<>>] ->
          Price = binary_to_float(Price_str),
          Amount = binary_to_integer(Amount_str),
          Time = dat_time_str_to_milliseconds(Dat_time_str, Prev_time),
          {#{price => Price, amount => Amount, time => Time}, {Time, <<>>}};
        _ ->
          {Bin_str, {Prev_time, Bin_str}}
      end
    end,

  Bin_list = binary:split(<<Chunk/bits>>, <<"\n">>, [global, trim]),
  Trimmed_bin_list =
    case Bin_list of
      [<<>> | Tail] ->
        Tail;
      _ ->
        Bin_list
    end,
  {Data, {Time, Leftover}} = lists:mapfoldl(Map_fun, {Last_tick_time, <<>>}, Trimmed_bin_list),
  Ticks =
    case Leftover of
      <<>> ->
        %%the read chunk is processed without left over
        Data;
      _ ->
        %%the last element of the list is new leftover and should be removed
        {V, _} = lists:split(length(Data) - 1, Data),
        V
    end,
  {ok, Ticks, Leftover, Time}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec dat_time_str_to_milliseconds(Bin_str :: binary(), Prev_time :: integer() | undefined) -> integer().
%% @doc 1)converts string of the following format:
%%           YYYY-MM-DD HH:MM:SS<.milisec>
%%        into a number of miliseconds from 1st Jan 1970
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
      Sec_since_0 = calendar:datetime_to_gregorian_seconds(Date_time),
      %%have to extract the seconds for 1970 Jan 1st and
      Seconds_since_1790 = Sec_since_0 -
        calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
      %%and add milliseconds
      Seconds_since_1790 * 1000 + Msec;
    {error, _Reson} ->
      Prev_time
  end.

-ifdef(TEST).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dat_time_str_to_miliseconds_test_() ->
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

extract_ticks_from_chunk_test_() ->
  P = 100.0,
  P_bin = float_to_binary(P),

  T_bin = <<"2010-01-11 10:30:00.080">>,
  T = dat_time_str_to_milliseconds(T_bin, 0),

  S_prefix = <<"RIH0;RTS-3.10;">>,
  S_suffix = <<";1;", T_bin/bits, ";129633008;0">>,
  S1 = <<S_prefix/bits, P_bin/bits, S_suffix/bits>>,
  {ok, [#{price := P}], Res_1, T} = extract_ticks_from_chunk(<<S1/bits, "\r">>, 0),
  {ok, [#{price := P1}], Res_2, T} = extract_ticks_from_chunk(<<S1/bits, "\r\n">>, 0),

  {ok, [], Res_3, 0} = extract_ticks_from_chunk(S_prefix, 0),
  {ok, [#{price := P1}], Res_4, T} = extract_ticks_from_chunk(<<S1/bits, "\r\n", S_prefix/bits>>, 0),
  {ok, [], Res_5, 0} = extract_ticks_from_chunk(S1, 0),
  {ok, [], Res_6, 0} = extract_ticks_from_chunk(<<"\n", S1/bits>>, 0),
  {ok, [], Res_7, 0} = extract_ticks_from_chunk(<<>>, 0),
  {ok, [], Res_8, 0} = extract_ticks_from_chunk(<<"\n">>, 0),
  {ok, [], Res_9, 0} = extract_ticks_from_chunk(<<"\r\n">>, 0),

  [
    ?_assertEqual(<<>>, Res_1),
    ?_assertEqual(<<>>, Res_2),
    ?_assertEqual(S_prefix, Res_3),
    ?_assertEqual(S_prefix, Res_4),
    ?_assertEqual(S1, Res_5),
    ?_assertEqual(S1, Res_6),
    ?_assertEqual(<<>>, Res_7),
    ?_assertEqual(<<>>, Res_8),
    ?_assertEqual(<<"\r">>, Res_9)
  ].

-endif.



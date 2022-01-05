%%%-------------------------------------------------------------------
%%% @author MNC834
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Dec 2021 9:23 PM
%%%-------------------------------------------------------------------
-module(times).
-author("MNC834").

%% calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).
-define(SECONDS_SINCE_1970, 62167219200).

%% API
-export([to_milliseconds_since_1970/1, to_milliseconds_since_1970/2]).

%% Types
-export_type([time/0]).

-type time() :: integer().
%% @doc milliseconds since 00:00 of 1st Jan 1970

-spec to_milliseconds_since_1970(calendar:datetime1970()) -> time().
%% @doc converts a date to milliseconds sice 00:00 Jan 1 1970
to_milliseconds_since_1970(Datetime) ->
  to_milliseconds_since_1970(Datetime, 0).

-spec to_milliseconds_since_1970(Datetime :: calendar:datetime1970(), Milliseconds :: integer()) -> time().
%% @doc converts a date + milliseconds to milliseconds sice 00:00 Jan 1 1970
to_milliseconds_since_1970(Datetime, Milliseconds) ->
  Sec_since_0 = calendar:datetime_to_gregorian_seconds(Datetime),
  %% have to extract the seconds for 1970 Jan 1st and
  Seconds_since_1790 = Sec_since_0 - ?SECONDS_SINCE_1970,
  %% and add milliseconds
  Seconds_since_1790 * 1000 + Milliseconds.
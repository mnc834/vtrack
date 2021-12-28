%%%-------------------------------------------------------------------
%%% @author MNC834
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Nov 2021 9:42 PM
%%%-------------------------------------------------------------------
-module(catalog_processor).
-author("MNC834").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(TRADES_PATTERN, <<"trades">>).
-define(CANDLES_PATTERN, <<"candles">>).

%% API
-export([catalog_map_fun/2]).

-export_type([catalog/0, data_type/0]).

-type data_type() :: trades | candles.

-type catalog_entry() :: #{name:= string(), file_name := string(), type := data_type()}.

-type catalog() :: [catalog_entry()].

-spec catalog_map_fun(file_reader:file_chunk(), undefined) -> Result
  when Result :: {{true, catalog_entry()} | false, Leftover :: file_reader:file_chunk(), undefined}.
%% @doc processes a binary string, extracts a catalog_entry and returns the extracted catalog_entry or
%%      the leftover that can't be processed.
catalog_map_fun(Chunk, undefined) ->
  case binary:split(Chunk, [<<";">>], [global]) of
    [Name, File_name, Type]
      when Type =:= ?TRADES_PATTERN ; Type =:= ?CANDLES_PATTERN ->
      {{true, #{name => binary_to_list(Name), file_name => binary_to_list(File_name), type => binary_to_atom(Type, latin1)}}, <<>>, undefined};
    _ ->
      {false, Chunk, undefined}
  end.

-ifdef(TEST).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
catalog_map_fun_test_() ->
  {{true, #{name := "STOCK", file_name := "STOCK.txt", type := Type1}}, <<>>, undefined} =
    catalog_map_fun(<<"STOCK;STOCK.txt;trades">>, undefined),
  {{true, #{name := "STOCK", file_name := "STOCK.txt", type := Type2}}, <<>>, undefined} =
    catalog_map_fun(<<"STOCK;STOCK.txt;candles">>, undefined),
  {false, <<"STOCK;STOCK.txt;some_type">>, undefined} =
    catalog_map_fun(<<"STOCK;STOCK.txt;some_type">>, undefined),

  [
    ?_assertEqual(trades, Type1),
    ?_assertEqual(candles, Type2)
  ].

-endif.

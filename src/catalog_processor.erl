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

%% API
-export([catalog_map_fun/2]).

-export_type([catalog/0]).

-type catalog_entry() :: #{file_name := binary()}.

-type catalog() :: [catalog_entry()].

-spec catalog_map_fun(file_reader:file_chunk(), undefined) -> Result
  when Result :: {{true, catalog_entry()} | false, Leftover :: file_reader:file_chunk(), undefined}.
%% @doc processes a binary string, extracts a catalog_entry and returns the extracted catalog_entry or
%%      the leftover that can't be processed.
catalog_map_fun(Chunk, undefined) ->
  case binary:split(Chunk, [<<";">>], [global]) of
    [Name] ->
      {{true, #{file_name => Name}}, <<>>, undefined};
    _ ->
      {false, Chunk, undefined}
  end.

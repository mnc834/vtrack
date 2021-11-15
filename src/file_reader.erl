%%%-------------------------------------------------------------------
%%% @author MNC834
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Nov 2021 12:23 PM
%%%-------------------------------------------------------------------
-module(file_reader).
-author("MNC834").

%% API
-export([open_file/2,
  read_next_chunk/1,
  close_file/1]).

-export_type([file_reader_data/0, file_chunk/0]).

-define(CHUNK_SIZE, 1000000).

-type file_chunk() :: binary().
%% @doc Chunk of data to be processed by file_processor.

-type file_data() :: undefined | term().
%% @doc A generic placeholder for the file_processor to keep its state for processing next chunks

-type processed_data() :: [term()].
%% @doc data extracted from the file

-type file_processor() :: fun((file_chunk(), file_data()) -> {ok, processed_data(), Leftover :: file_chunk(), file_data()}).
%% @doc File processor that processes binary data from the file

-record(file_reader_data, {
  file = undefined :: file:io_device() | undefined,
  leftover = <<>> :: binary(),
  file_data = undefined :: file_data(),
  file_processor :: file_processor()
}).

-type file_reader_data() :: #file_reader_data{}.

-spec open_file(Fname :: file:filename(), File_processor :: file_processor()) ->
  {ok, file_reader_data()} |
  {error, Reason :: term()}.
%% @doc opens a file with the given name and creates a file reader data with the given processor
open_file(Fname, File_processor)->
  case file:open(Fname, [read, binary]) of
    {ok, Fd} ->
      {ok, #file_reader_data{file = Fd, file_processor = File_processor}};
    Error ->
      Error
  end.

-spec read_next_chunk(file_reader_data()) ->
  {ok, processed_data(), file_reader_data()} |
  eof |
  {error, Reason :: term()}.
%% @reads data from the file. in case of eof or error, closes the file
read_next_chunk(#file_reader_data{file = undefined}) ->
  {error, file_not_opened};
read_next_chunk(#file_reader_data{file = Fd, file_processor = Processor,
                                  leftover =  Leftover, file_data = File_data} = File_reader_data) ->
  case file:read(Fd, ?CHUNK_SIZE) of
    {ok, Bin_data} ->
      case Processor(<<Leftover/bits, Bin_data/bits>>, File_data) of
        {ok, [], <<>>, _} ->
          eof;
        {ok, [], New_leftover, _} ->
          {error, {leftover_is_not_empty_when_no_ticks_extracted, New_leftover}};
        {ok, Processed_data, New_leftover, New_file_data} ->
          New_file_reader_data =
            File_reader_data#file_reader_data
            {
              leftover = New_leftover,
              file_data = New_file_data
            },
          {ok, Processed_data, New_file_reader_data}
      end;
    eof ->
      case Leftover of
        <<>> ->
          eof;
        _ ->
          {error, {leftover_is_not_empty_at_eof, Leftover}}
      end;
    {error, Reason} ->
      {error, {file_read_failed, Reason}}
  end.

-spec close_file(file_reader_data()) -> ok | {error, Reason :: term()}.
%% @doc closes a file
close_file(#file_reader_data{file = undefined}) ->
  {error, file_not_opened};
close_file(#file_reader_data{file = Fd})->
  file:close(Fd).
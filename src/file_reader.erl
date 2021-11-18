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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([open_file/2,
  read_next_chunk/1,
  close_file/1,
  generate_line_file_processor/1]).

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

-type line_map_fun() :: fun ((file_chunk(), file_data()) ->
                            {{true, Item :: term()} | false, Leftover :: file_chunk(), file_data()}).
%% @doc function used to parse lines, returns {true, Item} if it is able to parse the line or false otherwise

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
          {error, {leftover_is_not_empty_when_no_data_extracted, New_leftover}};
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

-spec generate_line_file_processor(Fun :: line_map_fun()) -> file_processor().
%% @doc generates a file processor that is meant to split file into lines and process them using Fun
generate_line_file_processor(Fun) ->
  fun(Chunk, File_data) ->
    process_lines(Chunk, File_data, Fun)
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec process_lines(file_chunk(), file_data(), line_map_fun()) -> Result
  when Result :: {ok, processed_data(), Leftover::file_chunk(), file_data()}.
%% @doc Splits the chunk into lines and processes lines using the given map function.
%% Lines should be separated by "\r\n" or "\n"
process_lines(Chunk, File_data, Map_fun) ->
  Map_fun_wrapper = fun(Bin_str, {<<>>, F_data}) ->
                  {Result, Leftover, New_file_data} = Map_fun(Bin_str, F_data),
                  {Result, {Leftover, New_file_data}}
                end,
  Bin_list = binary:split(<<Chunk/bits>>, [<<"\r\n">>, <<"\r">>, <<"\n">>], [global, trim]),
  {Data, {Leftover, New_file_data}} = filermapfoldl(Map_fun_wrapper, {<<>>, File_data}, Bin_list),
  {ok, Data, Leftover, New_file_data}.

-spec filermapfoldl(Fun, Acc0, List1) -> {List2, Acc1} when
  Fun :: fun((A, Acc_in) -> {boolean() | {true, B}, Acc_out}),
  Acc0 :: term(),
  Acc1 :: term(),
  Acc_in :: term(),
  Acc_out :: term(),
  List1 :: [A],
  List2 :: [B],
  A :: term(),
  B :: term().
%% @doc a combination of filtermap/2 and mapfoldl/3
filermapfoldl(Fun, Acc0, []) when is_function(Fun, 2)->
  {[], Acc0};
filermapfoldl(Fun, Acc0, [H | T]) ->
  {Result, Acc_tmp} = Fun(H, Acc0),
  {List_tmp, Acc1} = filermapfoldl(Fun, Acc_tmp, T),
  List2 =
    case Result of
      true ->
        [H | List_tmp];
      false ->
        List_tmp;
      {true, B} ->
        [B | List_tmp]
    end,
  {List2, Acc1}.

-ifdef(TEST).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filermapfoldl_test_() ->
  List0 = lists:seq(1, 10),
  Acc0 = 100,
  {List1, Acc0} = filermapfoldl(fun(_X, Acc) -> {true, Acc} end, Acc0, List0),
  {List2, Acc0} = filermapfoldl(fun(X, Acc) -> {{true, X}, Acc} end, Acc0, List0),
  {List3, Acc0} = filermapfoldl(fun(_X, Acc) -> {false, Acc} end, Acc0, List0),

  {List4, Acc0} = filermapfoldl(fun(X, Acc) when X > 5 -> {{true, 10 + X}, Acc}; (_X, Acc) -> {false, Acc} end, Acc0, List0),
  {List4, Acc1} = filermapfoldl(fun(X, Acc) when X > 5 -> {{true, 10 + X}, Acc + X}; (_X, Acc) -> {false, Acc} end, Acc0, List0),

  [
    ?_assertEqual(List0, List1),
    ?_assertEqual(List0, List2),
    ?_assertEqual([], List3),
    ?_assertEqual(lists:filtermap(fun(X) when X > 5 -> {true, X + 10}; (_) -> false end, List0), List4),
    ?_assertEqual(lists:foldl(fun(X, Acc) when X > 5 -> Acc + X; (_, Acc) -> Acc end, Acc0, List0), Acc1)
  ].

process_lines_test_()->
  Map_fun = fun(X, undefined) when is_binary(X) ->
              V = binary_to_list(X),
              if
                length(V) > 3 ->
                  {{true, V}, <<>>, undefined};
                true ->
                  {false, X, undefined}
              end
            end,

  Line1_str = "Line1",
  Line2_str = "Line2",

  Line1 = list_to_binary(Line1_str),
  Line2 = list_to_binary(Line2_str),
  Leftover = <<"Li">>,

  {ok, Result1, <<>>, undefined} = process_lines(<<Line1/bits, "\r\n", Line2/bits>>, undefined, Map_fun),
  {ok, Result1, <<>>, undefined} = process_lines(<<Line1/bits, "\n", Line2/bits>>, undefined, Map_fun),
  {ok, Result1, <<>>, undefined} = process_lines(<<"\n", Line1/bits, "\r\n", Line2/bits, "\r">>, undefined, Map_fun),
  {ok, Result1, Leftover4, undefined} = process_lines(<<"\n", Line1/bits, "\r\n", Line2/bits, "\r\n", Leftover/bits>>, undefined, Map_fun),

  [
    ?_assertEqual([Line1_str, Line2_str], Result1),
    ?_assertEqual(Leftover, Leftover4)
  ].

-endif.

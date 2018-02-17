%%%-------------------------------------------------------------------
%%% @author PKQ874
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. апр 2016 14:53
%%%-------------------------------------------------------------------
-module(sample_track).
-author("PKQ874").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-behaviour(gen_server).

-define(CHUNK_SIZE, 1000000).
-define(MEAN_SPAN, 60000).   %%in miliseconds

-type tick() :: #{price => float(), amount => integer(), time => integer()}.
%% @doc export variant for  tick_rec

-export_type([tick/0]).

%% API
-export([start_link/1,
  get_data/0,
  read_next_chunk/0,
  slice_ticks/2,
  slice_ticks_after/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(tick_rec, {
  price :: float(),
  amount :: integer(),
  time :: integer() %%miliseconds from 00:00:00 1st Jan 1970
}).

-record(file_data,{
  file :: file:io_device() | undefined,
  leftover = <<>> :: binary(),
  last_tick_time = undefined :: undefined | integer()
}).

-record(state, {
  file_data :: #file_data{},
  ticks = []:: [tick()]
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Fname :: string()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Fname) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Fname], []).

-spec get_data() -> {ok, [tick()]} | {error, Reason :: term()}.
%% @doc return data that the sample track holds
get_data() ->
  gen_server:call(?SERVER, get_data).

-spec read_next_chunk() -> {ok, [tick()]} | eof | {error, Reason :: term}.
%% @doc reads the next chunk of data from the file
%% appends the data to the state list, returns the read data
%% if no more data to read returns eof and closes the file
%% returns error if read failes
read_next_chunk() ->
  gen_server:call(?SERVER, read_next_chunk).

-spec slice_ticks(From :: integer(), To :: integer()) -> [tick()].
%% @doc extracts ticks which have time within [From, To]
slice_ticks(From, To) ->
  gen_server:call(?SERVER, {slice_ticks, From, To}).

-spec slice_ticks_after(From :: integer()) -> [tick()].
%% @doc extracts ticks which have time within more than FRrom
slice_ticks_after(From) ->
  gen_server:call(?SERVER, {slice_ticks_after, From}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Fname]) ->
  %% opening the file
  Result =
    case application:get_env(tick_samples_dir) of
      {ok, Dir} ->
        Full_path = Dir ++ "/" ++ Fname,
        case file:open(Full_path, [read, binary]) of
          {ok, Fd} ->
            {ok, #state{file_data = #file_data{file = Fd}}};
          {error, Reason} ->
            {error, {open_file_failed, Full_path, Reason}}
        end;
      undefined ->
        {error, {getting_tick_samples_dir_failed}}
    end,
  case Result of
    {ok, #state{}} ->
      Result;
    {error, _} ->
      {stop, Result}
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(get_data, _From, #state{ticks = []} = State) ->
  {reply, {error, data_is_not_initialized}, State};
handle_call(get_data, _From, #state{ticks = Deals} = State) ->
  {reply, {ok, Deals}, State};
handle_call(read_next_chunk, _From, #state{file_data = #file_data{file = undefined}} = State) ->
  {reply, {error, file_is_closed}, State};
handle_call(read_next_chunk, _From, #state{file_data = File_data, ticks = Deals} = State) ->
  case extract_chunk_from_file(File_data) of
    {ok, Chunk, New_file_data} ->
      Dec_chunk = decimate_ticks(Chunk),
      Deals_chunk = tick_recs_to_ticks(Dec_chunk),
      New_state =
        State#state{
          ticks = Deals ++ Deals_chunk,
          file_data = New_file_data
        },
      {reply, {ok, Deals_chunk}, New_state};
    Result ->
      %%either eof or {error, Error}
      #file_data{file = Fd} = File_data,
      ok = file:close(Fd),
      {reply, Result, State#state{file_data = File_data#file_data{file = undefined}}}
  end;
handle_call({slice_ticks, From, To}, _From, #state{ticks = Deals} = State) ->
  From_pred = gen_before_t_predicate(From),
  To_pred = gen_before_t_predicate(To),
  L_1 = lists:dropwhile(From_pred, Deals),
  L = lists:takewhile(To_pred, L_1),
  {reply, L, State};
handle_call({slice_ticks_after, From}, _From, #state{ticks = Deals} = State) ->
  From_pred = gen_before_t_predicate(From),
  L = lists:dropwhile(From_pred, Deals),
  {reply, L, State}.





%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec extract_chunk_from_file(File_data :: #file_data{}) -> Result
  when  Result :: {ok, [#tick_rec{}], New_file_data :: #file_data{}} |
                 eof |
                 {error, Reason :: term()}.
%% @doc Extracts next chunk of data from the opened file
extract_chunk_from_file(#file_data{file = Fd, leftover = Leftover,
  last_tick_time = Last_tick_time} = File_data) ->

  case file:read(Fd, ?CHUNK_SIZE) of
    {ok, Bin_data} ->
      case extract_ticks_from_chunk(<<Leftover/bits, Bin_data/bits>>, Last_tick_time) of
        {[], <<>>, _} ->
          eof;
        {[], New_leftover, _} ->
          {error, {leftover_is_not_empty_when_no_ticks_extracted, New_leftover}};
        {Ticks, New_leftover, New_last_tick_time} ->
          New_file_data =
            File_data#file_data
            {
              leftover = New_leftover,
              last_tick_time = New_last_tick_time
            },
          {ok, Ticks, New_file_data}
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

-spec extract_ticks_from_chunk(Chunk :: binary(), Last_tick_time :: integer()) -> Result
  when Result :: {[#tick_rec{}], Leftover :: binary(), Last_time :: integer()}.
%% @doc processes chunk or binary, extracts all the ticks and returns the extracted ticks and
%%      the leftover that can't be processed. Ticks should be separated by "\r\n"
extract_ticks_from_chunk(Chunk, Last_tick_time) ->
  Map_fun =
    fun(Bin_str, {Prev_time, <<>>}) ->
      case binary:split(Bin_str, [<<";">>, <<"\r">>], [global]) of
        [_Code, _Contract, Price_str, Amount_str, Dat_time_str, _Trade_id, _Nosystem, <<>>] ->
          Price = binary_to_float(Price_str),
          Amount = binary_to_integer(Amount_str),
          Time = dat_time_str_to_miliseconds(Dat_time_str, Prev_time),
          {#tick_rec{price = Price, amount = Amount, time = Time}, {Time, <<>>}};
        _ ->
          {Bin_str, {Prev_time, Bin_str}}
      end
    end,

  Bin_list = binary:split(<<Chunk/bits>>, <<"\n">>, [global, trim]),
  Trimmed_bin_lest =
    case Bin_list of
      [<<>> | Tail] ->
        Tail;
      _ ->
        Bin_list
    end,
  {Data, {Time, Leftover}} = lists:mapfoldl(Map_fun, {Last_tick_time, <<>>}, Trimmed_bin_lest),
  Ticks =
    case Leftover of
      <<>> ->
        %%the read chunk is processed without left over
        Data;
      _ ->
        %%the last elemetn of the list is new leftover and should be removed
        {V, _} = lists:split(length(Data) - 1, Data),
        V
    end,
  {Ticks, Leftover, Time}.


-spec dat_time_str_to_miliseconds(Bin_str :: binary(), Prev_time :: integer() | undefined) -> integer().
%% @doc 1)converts string of the following format:
%%           YYYY-MM-DD HH:MM:SS<.milisec>
%%        into a number of miliseconds from 1st Jan 1970
%%      2) if Bin_str contains the following format:
%%           DD.MM.YYYY HH:MM
%%         and Prev_time =:= undefined
%%         converts Bin_str to miliseconds from 1st Jan 1970
%%      3) In all other cases
%%      the Prev_time is returned
dat_time_str_to_miliseconds(Bin_str, Prev_time) ->
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
      %%have to extarct the seconds for 1970 Jan 1st and
      Seconds_since_1790 = Sec_since_0 -
        calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
      %%and add miliseconds
      Seconds_since_1790 * 1000 + Msec;
    {error, _Reson} ->
      Prev_time
  end.

-spec tick_recs_to_ticks([#tick_rec{}]) -> [tick()].
%% @converts #tick_rec to deal maps
tick_recs_to_ticks(Deal_recs) ->
  Map_fun =
    fun(#tick_rec{price = Price, amount = Amount, time = Time}) ->
      #{price => Price, amount => Amount, time => Time}
    end,
  lists:map(Map_fun, Deal_recs).

-spec gen_before_t_predicate(T :: integer()) -> fun((#tick_rec{} | tick()) -> boolean()).
%% @doc generates a function that serves as a predicate the returns true if given tick's time
%%      is less then T and false otherwise
gen_before_t_predicate(T_limit) ->
  fun(#tick_rec{time = T}) when T < T_limit ->
    true;
    (#{time := T}) when T < T_limit ->
      true;
    (_) ->
      false
  end.

add_tick(#tick_rec{price = P, amount = A}, {#tick_rec{price = P_s, amount = A_s} = S, N}) ->
  {S#tick_rec{price = P_s + P, amount = A_s + A}, N + 1}.

-spec decimate_ticks(Ticks :: [#tick_rec{}]) -> [#tick_rec{}].
%% @doc decimates the tick list using a mean value in a ?MEAN_SPAN interval
decimate_ticks([]) ->
  [];
decimate_ticks([#tick_rec{time = T_start} | _] = Ticks) ->
  Pred = gen_before_t_predicate(T_start + ?MEAN_SPAN),
  {L, Rest} = lists:splitwith(Pred, Ticks),
  {#tick_rec{price = P_s, amount = A_s}, N} =
    lists:foldl(fun add_tick/2, {#tick_rec{price = 0.0, amount = 0, time = 0}, 0}, L),
  [#tick_rec{price = P_s / N, amount = round(A_s / N), time = T_start} | decimate_ticks(Rest)].


-ifdef(TEST).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dat_time_str_to_miliseconds_test_() ->
  T_bin_1 =  <<"2010-01-11 10:30:00.080">>,
  T_bin_2 = <<"11.01.2010 10:30">>,

  Res_1 = dat_time_str_to_miliseconds(T_bin_1, 0),
  Res_2 = dat_time_str_to_miliseconds(T_bin_1, undefined),
  Res_3 = dat_time_str_to_miliseconds(T_bin_2, 0),
  Res_4 = dat_time_str_to_miliseconds(T_bin_2, undefined),

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
  T = dat_time_str_to_miliseconds(T_bin, 0),

  S_prefix = <<"RIH0;RTS-3.10;">>,
  S_suffix = <<";1;", T_bin/bits, ";129633008;0">>,
  S1 = <<S_prefix/bits, P_bin/bits, S_suffix/bits>>,
  {[#tick_rec{price = P}], Res_1, T} = extract_ticks_from_chunk(<<S1/bits, "\r">>, 0),
  {[#tick_rec{price = P1}], Res_2, T} = extract_ticks_from_chunk(<<S1/bits, "\r\n">>, 0),

  {[], Res_3, 0} = extract_ticks_from_chunk(S_prefix, 0),
  {[#tick_rec{price = P1}], Res_4, T} = extract_ticks_from_chunk(<<S1/bits, "\r\n", S_prefix/bits>>, 0),
  {[], Res_5, 0} = extract_ticks_from_chunk(S1, 0),
  {[], Res_6, 0} = extract_ticks_from_chunk(<<"\n", S1/bits>>, 0),
  {[], Res_7, 0} = extract_ticks_from_chunk(<<>>, 0),
  {[], Res_8, 0} = extract_ticks_from_chunk(<<"\n">>, 0),
  {[], Res_9, 0} = extract_ticks_from_chunk(<<"\r\n">>, 0),

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
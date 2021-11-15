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

-behaviour(gen_server).

-define(MEAN_SPAN, 60000).   %%in miliseconds

%% API
-export([start_link/0,
  get_data/0,
  open_file/1,
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

-type ticks() :: trades_processor:ticks().

-record(state, {
  file_data = undefined :: file_reader:file_reader_data() | undefined,
  ticks = []:: ticks()
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
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_data() -> {ok, ticks()} | {error, Reason :: term()}.
%% @doc return data that the sample track holds
get_data() ->
  gen_server:call(?SERVER, get_data).

-spec open_file(Fname :: string()) -> ok | {error, Reason :: term()}.
%% @doc opens a file with the given name in the configured directory
%% and resets all the previous data.
open_file(Fname) ->
  gen_server:call(?SERVER, {open_file, Fname}).

-spec read_next_chunk() -> {ok, ticks()} | eof | {error, Reason :: term()}.
%% @doc reads the next chunk of data from the file
%% appends the data to the state list, returns the read data
%% if no more data to read returns eof and closes the file
%% returns error if read fails
read_next_chunk() ->
  gen_server:call(?SERVER, read_next_chunk).

-spec slice_ticks(From :: integer(), To :: integer()) -> ticks().
%% @doc extracts ticks which have time within [From, To]
slice_ticks(From, To) ->
  gen_server:call(?SERVER, {slice_ticks, From, To}).

-spec slice_ticks_after(From :: integer()) -> ticks().
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
init([]) ->
  {ok, #state{}}.

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
handle_call({open_file, Fname}, _From, #state{file_data = undefined}) ->
  %% openning the file
  case application:get_env(tick_samples_dir) of
    {ok, Dir} ->
      Full_path = Dir ++ "/" ++ Fname,
      case file_reader:open_file(Full_path, fun trades_processor:extract_ticks_from_chunk/2) of
        {ok, File_data} ->
          {reply, ok, #state{file_data = File_data}};
        Error ->
          {reply, Error, #state{}}
      end;
    undefined ->
      {error, {getting_tick_samples_dir_failed}}
  end;
handle_call({open_file, Fname}, From, #state{file_data = File_data} = State) ->
  %% closing the file first
  case file_reader:close_file(File_data) of
    ok ->
      handle_call({open_file, Fname}, From, State#state{file_data = undefined});
    Error ->
      {reply, Error, #state{}}
  end;
handle_call(read_next_chunk, _From, #state{file_data = undefined} = State) ->
  {reply, {error, file_is_closed}, State};
handle_call(read_next_chunk, _From, #state{file_data = File_data, ticks = Deals} = State) ->
  case file_reader:read_next_chunk(File_data) of
    {ok, Chunk, New_file_data} ->
      Dec_chunk = decimate_ticks(Chunk),
      New_state =
        State#state{
          ticks = Deals ++ Dec_chunk,
          file_data = New_file_data
        },
      {reply, {ok, Dec_chunk}, New_state};
    Result ->
      %%either eof or {error, Error}
      ok  = file_reader:close_file(File_data),
      {reply, Result, State#state{file_data = undefined}}
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


-spec gen_before_t_predicate(T :: integer()) -> fun((trades_processor:tick()) -> boolean()).
%% @doc generates a function that serves as a predicate the returns true if given tick's time
%%      is less then T and false otherwise
gen_before_t_predicate(T_limit) ->
  fun (#{time := T}) when T < T_limit ->
      true;
    (_) ->
      false
  end.

add_tick(#{price := P, amount := A}, {#{price := P_s, amount := A_s}, N}) ->
  {#{price => P_s + P, amount => A_s + A}, N + 1}.

-spec decimate_ticks(Ticks :: ticks()) -> ticks().
%% @doc decimates the tick list using a mean value in a ?MEAN_SPAN interval
decimate_ticks([]) ->
  [];
decimate_ticks([#{time := T_start} | _] = Ticks) ->
  Pred = gen_before_t_predicate(T_start + ?MEAN_SPAN),
  {L, Rest} = lists:splitwith(Pred, Ticks),
  {#{price := P_s, amount := A_s}, N} =
    lists:foldl(fun add_tick/2, {#{price => 0.0, amount => 0, time => 0}, 0}, L),
  [#{price => P_s / N, amount => round(A_s / N), time => T_start} | decimate_ticks(Rest)].
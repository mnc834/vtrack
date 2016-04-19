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

-define(SAMPLE_DIR, "samples").

-type deal() :: #{price => float(), amount => integer(), time => integer()}.
%% @doc export variant for  deal_rec

-export_type([deal/0]).

%% API
-export([start_link/1,
  get_data/0,
  get_js_encoded_data/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(deal_rec, {
  price :: float(),
  amount :: integer(),
  time :: integer() %%miliseconds from 00:00:00 1st Jan 1970
}).

-record(state, {
  data :: [#deal_rec{}],
  deals :: [deal()],
  js_data :: jsx:json_text()
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

-spec get_data() -> {ok, [deal()]} | {error, Reason :: term()}.
%% @doc return data that the sample track holds
get_data() ->
  gen_server:call(?SERVER, get_data).

-spec get_js_encoded_data() -> {ok, jsx:json_text()} | {error, Reason :: term()}.
%% @returns data in the form of encoded list of JSON deal() objects
get_js_encoded_data() ->
  gen_server:call(?SERVER, get_js_encoded_data).

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
  case extract_data_from_file(Fname) of
    {ok, Data} ->
      Deals = deal_recs_to_deals(Data),
      Js_data = jsx:encode(Deals),
      {ok, #state{data = Data, deals = Deals, js_data = Js_data}};
    {error, Reason} ->
      {stop, {extract_data_from_file, Reason}}
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
handle_call(get_data, _From, #state{deals = undefined} = State) ->
  {reply, {error, data_is_not_initialized}, State};
handle_call(get_data, _From, #state{deals = Deals} = State) ->
  {reply, {ok, Deals}, State};
handle_call(get_js_encoded_data, _From, #state{js_data = undefined} = State) ->
  {reply, {error, data_is_not_initialized}, State};
handle_call(get_js_encoded_data, _From, #state{js_data = Js_data} = State) ->
  {reply, {ok, Js_data}, State}.

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
-spec extract_data_from_file(Fname :: string()) -> {ok, [#deal_rec{}]} | {error, Reason :: term()}.
%% @doc Extracts data from a given file stored in priv directory
extract_data_from_file(Fname) ->
  Map_fun =
    fun(Bin_str, Prev_date) ->
      [_Code, _Contract, Price_str, Amount_str, Dat_time_str, _Trade_id, _Nosystem] =
        binary:split(Bin_str, <<";">>, [global]),
      Price = binary_to_float(Price_str),
      Amount = binary_to_integer(Amount_str),
      Time = dat_time_str_to_miliseconds(Dat_time_str, Prev_date),
      {#deal_rec{price = Price, amount = Amount, time = Time}, Time}
    end,

  case application:get_application(self()) of
    {ok, App_name} ->
      case code:priv_dir(App_name) of
        Priv_dir_path when is_list(Priv_dir_path) ->
          Full_path = Priv_dir_path ++ "/" ++ ?SAMPLE_DIR ++ "/" ++ Fname,
          case file:read_file(Full_path) of
            {ok, Bin_data} ->
              Bin_list = binary:split(Bin_data, <<"\r\n">>, [global, trim]),
              {Data, _Acc_out} = lists:mapfoldl(Map_fun, 0, Bin_list),
              {ok, Data};
            {error, Reason} ->
              {error, {read_file_failed, Full_path, Reason}}
          end;
        {error, Reason} ->
          {error, {priv_dir_failed, Reason}}
      end;
    undefined ->
      {error, get_application_failed}
  end.

-spec dat_time_str_to_miliseconds(Bin_str :: binary(), Prev_time :: integer()) -> integer().
%% @doc converts string of the following format:
%%           YYYY-MM-DD HH:MM:SS<.milisec>
%%      into a number of miliseconds from 1st Jan 1970
%%      in case of any other format
%%      the Prev_time is returned
dat_time_str_to_miliseconds(Bin_str, Prev_time) ->
  [Date_str, Time_str] = binary:split(Bin_str, <<" ">>),
  case binary:split(Date_str, <<"-">>, [global]) of
    [Year_str, Month_str, Day_str] ->
      case binary:split(Time_str, [<<":">>, <<".">>], [global]) of
        [Hour_str, Min_str, Sec_str, Msec_str] ->
          [Year, Month, Day, Hour, Min, Sec, Msec] =
            [binary_to_integer(X) ||
              X <- [Year_str, Month_str, Day_str, Hour_str, Min_str, Sec_str, Msec_str]],
          Date_time = {{Year, Month, Day}, {Hour, Min, Sec}},
          Sec_since_0 = calendar:datetime_to_gregorian_seconds(Date_time),
          %%have to extarct the seconds for 1970 Jan 1st and
          Seconds_since_1790 = Sec_since_0 -
            calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
          %%and add miliseconds
          Seconds_since_1790 * 1000 + Msec;
        _ ->
          Prev_time
      end;
    _ ->
      Prev_time
  end.

-spec deal_recs_to_deals([#deal_rec{}]) -> [deal()].
%% @converts #deal_rec to deal maps
deal_recs_to_deals(Deal_recs) ->
  Map_fun =
    fun(#deal_rec{price = Price, amount = Amount, time = Time}) ->
      #{price => Price, amount => Amount, time => Time}
    end,
  lists:map(Map_fun, Deal_recs).
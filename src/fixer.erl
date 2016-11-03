-module(fixer).
-behaviour(gen_server).
-include("../include/fixer.hrl").

%% API
-export([
         start_link/0
         , start/0
         , convert/3
         , convert/4
         , rates/0
         , rates/1
        ]).

%% gen_server callbacks
-export([init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3]).

-define(SERVER, ?MODULE).

-type currency() :: string() | binary().
-type amount() :: float() | integer().
-type options() :: map() | [{atom(), term()}].
-type rates() :: #{currency() => float()}.

% @doc
% Start fixerio application
% @end
-spec start() -> {ok, [atom()]} | {error, term()}.
start() ->
  application:ensure_all_started(?MODULE).

% @equiv convert(Amount, FromCurrency, ToCurrency, #{})
-spec convert(Amount :: amount(), FromCurrency :: currency(), ToCurrency :: currency()) ->
  {ok, NewAmount :: amount()}
  | {error, rates_not_available}.
convert(Amount, FromCurrency, ToCurrency) ->
  convert(Amount, FromCurrency, ToCurrency, #{}).

% @doc
% Convert the given <tt>Amount</tt> from <tt>FromCurrency</tt> to <tt>ToCurrency</tt>
%
% Options:
% <ul>
% <li><tt>at :: date()</tt> : Use rates at the given date (default: today).</li>
% <li><tt>base :: currency()</tt> : Quote against a given currency (default: <tt>&lt;&lt;"EUR"&gt;&gt;</tt>).</li>
% <li><tt>precision :: integer()</tt> : Response precision (default: 4).</li>
% </ul>
% @end
-spec convert(Amount :: amount(), FromCurrency :: currency(), ToCurrency :: currency(), Options :: options()) ->
  {ok, NewAmount :: amount()}
  | {error, unavailable_from_currency}
  | {error, unavailable_to_currency}
  | {error, rates_not_available}.
convert(Amount, FromCurrency, ToCurrency, Options) when is_map(Options) ->
  gen_server:call(?MODULE, {convert, bucs:to_float(Amount), bucs:to_binary(FromCurrency), bucs:to_binary(ToCurrency), Options});
convert(Amount, FromCurrency, ToCurrency, Options) when is_list(Options) ->
  convert(Amount, FromCurrency, ToCurrency, maps:from_list(Options)).

% @equiv rates(#{})
-spec rates() -> [currency()].
rates() ->
  rates(#{}).

% @doc
% Return the list of all availables rates
%
% Options:
% <ul>
% <li><tt>at :: date()</tt> : Use rates at the given date (default: today).</li>
% <li><tt>base :: currency()</tt> : Quote against a given currency (default: <tt>&lt;&lt;"EUR"&gt;&gt;</tt>).</li>
% </ul>
% @end
-spec rates(Options :: options()) -> rates().
rates(Options) when is_map(Options) ->
  gen_server:call(?MODULE, {rates, Options});
rates(Options) when is_list(Options) ->
  rates(maps:from_list(Options)).

% @hidden
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @hidden
init(_) ->
  Table = ets:new(fixerio, [private, named_table]),
  Base = doteki:get_as_binary([fixerio, base], ?BASE),
  preload(Table, Base),
  {ok, #{table => Table,
         base => Base}}.

% @hidden
handle_call({convert, Amount, FromCurrency, ToCurrency, Options}, _From, #{table := Table, base := Base} = State) ->
  Date = bucs:to_binary(maps:get(at, Options, bucdate:format("Y-m-d", bucdate:today_utc()))),
  UsedBase = bucs:to_binary(maps:get(base, Options, Base)),
  Precision = maps:get(precision, Options, ?PRECISION),

  Rates = find_rates(Date, UsedBase, Table),
  case {get_rate(Rates, FromCurrency), get_rate(Rates, ToCurrency)} of
    {error, _} ->
      {reply, {error, unavailable_from_currency}, State};
    {_, error} ->
      {reply, {error, unavailable_to_currency}, State};
    {RateFrom, RateTo} ->
      {reply, {ok, to_float(RateTo * (Amount / RateFrom), Precision)}, State}
  end;
handle_call({rates, Options}, _From, #{table := Table, base := Base} = State) ->
  Date = bucs:to_binary(maps:get(at, Options, bucdate:format("Y-m-d", bucdate:today_utc()))),
  UsedBase = bucs:to_binary(maps:get(base, Options, Base)),
  #{<<"rates">> := Rates} = find_rates(Date, UsedBase, Table),
  {reply, {Rates}, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

preload(Table, Base) ->
  case doteki:get_env([fixerio, preload]) of
    undefined ->
      ok;
    {From, today} ->
      preload(Table, Base,
              parse_date(From),
              today());
    {From, To} ->
      preload(Table, Base,
              parse_date(From),
              parse_date(To))
  end.

parse_date(Date) ->
  {D, _} = bucdate:parse(bucs:to_string(Date)),
  {D, {0, 0, 0}}.

today() ->
  {D, _} = bucdate:today_utc(),
  {D, {0, 0, 0}}.

preload(Table, Base, From, To) ->
  case bucdate:compare(From, To) of
    -1 ->
      io:format("==> preload done!~n"),
      ok;
    _ ->
      io:format("==> preload ~p~n", [From]),
      get_rates(Base, bucdate:format("Y-m-d", From), Table),
      preload(Table, Base, bucdate:add(From, 1, days), To)
  end.

find_rates(Date, Base, Table) ->
  case ets:lookup(Table, {Date, Base}) of
    [{{Date, Base}, Rates}] ->
      Rates;
    [] ->
      get_rates(Base, Date, Table)
  end.

get_rates(Base, Date, Table) ->
  case httpc:request(get, {bucs:to_string(<<?FIXERIO_URL/binary,
                                            (bucs:to_binary(Date))/binary,
                                            "?base=", Base/binary>>), []}, [], []) of
    {ok, {{_, 200, _}, _, Body}} ->
      #{<<"date">> := RDate} = Data = jsx:decode(bucs:to_binary(Body), [return_maps]),
      ets:insert(Table, {{RDate, Base}, Data}),
      Data;
    _ ->
      {stop, service_unavailable}
  end.

get_rate(Rates, Currency) ->
  case Rates of
    #{<<"base">> := Currency} ->
      1;
    #{<<"rates">> := Rates0} ->
      maps:get(Currency, Rates0, error)
  end.

to_float(Value, Precision) ->
  bucs:to_float(float_to_list(Value, [{decimals, Precision}])).


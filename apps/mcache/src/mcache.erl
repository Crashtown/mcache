%%%-------------------------------------------------------------------
%% @doc mcache top module.
%% @end
%%%-------------------------------------------------------------------

-module(mcache).

-behaviour(gen_server).

%% API
-export([start_link/0, set/2,
                       get/1,
                       gets/1,
                       delete/1,
                       add/2,
                       replace/2,
                       append/2,
                       prepend/2]).

%% Gen Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(mcache_key_value, {
  key :: binary(),
  value :: binary()
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
set(Key, Value) -> gen_server:cast(?SERVER, {set, {Key, Value}}).
get(Key) -> gen_server:call(?SERVER, {get, Key}).
gets(Keys) -> lists:map(fun get/1, Keys).
delete(Key) -> gen_server:call(?SERVER, {delete, Key}).
add(Key, Value) -> gen_server:call(?SERVER, {add, {Key, Value}}).
replace(Key, Value) -> gen_server:call(?SERVER, {replace, {Key, Value}}).
append(Key, Value) -> gen_server:call(?SERVER, {append, {Key, Value}}).
prepend(Key, Value) -> gen_server:call(?SERVER, {prepend, {Key, Value}}).

%%====================================================================
%% Gen server callbacks
%%====================================================================

init([]) ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(mcache_key_value, [{attributes, record_info(fields, mcache_key_value)}]),
  {ok, nostate}.

handle_cast({set, {Key, Value}}, State) ->
  Fun = fun() -> mnesia:write(#mcache_key_value{key = Key, value = Value}) end,
  mnesia:transaction(Fun),
  {noreply, State};

handle_cast(_Any, State) ->
  {noreply, State}.

handle_call({get, Key}, _From, State) ->
  Fun = fun() ->
      [#mcache_key_value{key = Key, value = Val}] = mnesia:read(mcache_key_value, Key), Val
  end,
  Reply = case mnesia:transaction(Fun) of
      {atomic, Value}    -> {Key, Value};
      {aborted, _Reason} -> {Key, not_found}
  end,
  {reply, Reply, State};

handle_call({delete, Key}, _From, State) ->
  Fun = fun () ->
    case mnesia:read(mcache_key_value, Key) of
      [#mcache_key_value{key = Key} = Record] -> mnesia:delete_object(Record), ok;
      [] -> not_found
    end
  end,
  {atomic, Reply} = mnesia:transaction(Fun),
  {reply, Reply, State};

handle_call({add, {Key, Value}}, _From, State) ->
  Fun = fun () ->
    case mnesia:read(mcache_key_value, Key) of
      [#mcache_key_value{key = Key}] -> exists;
      [] ->
        mnesia:write(#mcache_key_value{key = Key, value = Value}), ok
    end
  end,
  {atomic, Reply} = mnesia:transaction(Fun),
  {reply, Reply, State};

handle_call({replace, {Key, Value}}, _From, State) ->
  Fun = fun () ->
    case mnesia:read(mcache_key_value, Key) of
      [] -> not_found;
      [#mcache_key_value{key = Key}] ->
        mnesia:write(#mcache_key_value{key = Key, value = Value}), ok
    end
  end,
  {atomic, Reply} = mnesia:transaction(Fun),
  {reply, Reply, State};

handle_call({append, {Key, Value}}, _From, State) ->
  Fun = fun () ->
    case mnesia:read(mcache_key_value, Key) of
      [] -> not_found;
      [#mcache_key_value{key = Key, value = PreValue}] ->
        NewValue = list_to_binary([PreValue, Value]),
        mnesia:write(#mcache_key_value{key = Key, value = NewValue}), ok
    end
  end,
  {atomic, Reply} = mnesia:transaction(Fun),
  {reply, Reply, State};

handle_call({prepend, {Key, Value}}, _From, State) ->
  Fun = fun () ->
    case mnesia:read(mcache_key_value, Key) of
      [] -> not_found;
      [#mcache_key_value{key = Key, value = AfterValue}] ->
        NewValue = list_to_binary([Value, AfterValue]),
        mnesia:write(#mcache_key_value{key = Key, value = NewValue}), ok
    end
  end,
  {atomic, Reply} = mnesia:transaction(Fun),
  {reply, Reply, State};
handle_call(_Any, _From, State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

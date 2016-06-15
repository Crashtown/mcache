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

-define(SERVER, ?MODULE).

-record(state, {
  table :: ets:tid()
}).

%%====================================================================
%% API functions
%%====================================================================

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
set(Key, Value) -> gen_server:cast(?SERVER, {set, Key, Value}).
get(Key) -> gen_server:call(?SERVER, {get, Key}).
gets(Keys) -> lists:map(fun get/1, Keys).
delete(Key) -> gen_server:call(?SERVER, {delete, Key}).
add(Key, Value) -> gen_server:call(?SERVER, {add, Key, Value}).
replace(Key, Value) -> gen_server:call(?SERVER, {replace, Key, Value}).
append(Key, Value) -> gen_server:call(?SERVER, {append, Key, Value}).
prepend(Key, Value) -> gen_server:call(?SERVER, {prepend, Key, Value}).

%%====================================================================
%% Gen server callbacks
%%====================================================================

init([]) ->
    Table = ets:new(?MODULE, [set, protected, named_table]),
    {ok, #state{table = Table}}.

handle_cast({set, Key, Value}, #state{table = Table} = State) ->
    ets:insert(Table, {Key, Value}),
    {noreply, State};
handle_cast(_Any, State) ->
    {noreply, State}.

handle_call({get, Key}, _From, #state{table = Table} = State) ->
    Reply = case ets:lookup(Table, Key) of
        [] -> {Key, not_found};
        [{Key, Value}] -> {Key, Value}
    end,
    {reply, Reply, State};
handle_call({delete, Key}, _From, #state{table = Table} = State) ->
    Reply = case ets:lookup(Table, Key) of
        [] -> not_found;
        [{Key, Value}] ->
            ets:delete(Table, Key), ok
    end,
    {reply, Reply, State};
handle_call({add, Key, Value}, _From, #state{table = Table} = State) ->
    Reply = case ets:insert_new(Table, {Key, Value}) of
        true -> ok;
        false -> exists
    end,
    {reply, Reply, State};
handle_call({replace, Key, Value}, _From, #state{table = Table} = State) ->
    Reply = case ets:lookup(Table, Key) of
        [] -> not_found;
        [{Key, _}] -> ets:insert(Table, {Key, Value}), ok
    end,
    {reply, Reply, State};
handle_call({append, Key, Value}, _From, #state{table = Table} = State) ->
    Reply = case ets:lookup(Table, Key) of
        [] -> not_found;
        [{Key, PreValue}] ->
            ets:insert(Table, {Key, list_to_binary([PreValue, Value])}), ok
    end,
    {reply, Reply, State};
handle_call({prepend, Key, Value}, _From, #state{table = Table} = State) ->
    Reply = case ets:lookup(Table, Key) of
        [] -> not_found;
        [{Key, AfterValue}] ->
            ets:insert(Table, {Key, list_to_binary([Value, AfterValue])}), ok
    end,
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

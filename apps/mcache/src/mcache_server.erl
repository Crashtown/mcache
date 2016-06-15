%%%-------------------------------------------------------------------
%% @doc mcache TCP server.
%% @end
%%%-------------------------------------------------------------------

-module(mcache_server).

-behaviour(gen_server).

%% API
-export([start_link/0, accept/2]).

%% Gen Server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

accept(Id, ListenSocket) ->
    io:format("MS#~p] [info] Wait for client~n", [Id]),
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} -> handle_connection(Id, ListenSocket, Socket);
        {error, Reason} ->
            io:format("[MS#~p] [Error] ~p~n", [Id, Reason]),
            accept(Id, ListenSocket)
    end.

%%====================================================================
%% Gen server callbacks
%%====================================================================

init([]) ->
    {ok, PoolSize} = application:get_env(accept_pool_size),
    {ok, Port} = application:get_env(port),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet, line}, {reuseaddr, true}]),
    [spawn(?MODULE, accept, [Id, ListenSocket]) || Id <- lists:seq(1, PoolSize)],
    {ok, ListenSocket}.

handle_call(_Any, _From, State) ->
    {noreply, State}.

handle_cast(_Any, State) ->
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

handle_connection(Id, ListenSocket, Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {error, Reason} ->
            io:format("[MS#~p] [Error] ~p~n", [Id, Reason]),
            accept(Id, ListenSocket);
        {ok, Msg0} ->
            Message = binary:part(Msg0, 0, byte_size(Msg0) - 2),
            io:format("[MS#~p] [info] [msg] ~p~n", [Id, Message]),
            {ok, Response} = handle_protocol(Message),
            gen_tcp:send(Socket, <<Response/binary, "\r\n">>),
            handle_connection(Id, ListenSocket, Socket)
    end.

handle_protocol(Message) ->
    handle_message(binary:split(Message, <<" ">>)).

handle_message([<<"SET">>, Args]) ->
    [Key, Value] = args(Args),
    ok = mcache:set(Key, Value),
    {ok, <<"STORED">>};
handle_message([<<"GET">>, Key]) ->
    Response = case mcache:get(Key) of
        {Key, not_found} -> <<"NOT FOUND">>;
        {Key, Value} -> iolist_to_binary([kv_template({Key, Value}), "END"])
    end,
    {ok, Response};
handle_message([<<"GETS">>, Args]) ->
    Keys = binary:split(Args, <<" ">>, [global]),
    KeyValues = mcache:gets(Keys),
    Response = iolist_to_binary([lists:map(fun kv_template/1, KeyValues), "END"]),
    {ok, Response};
handle_message([<<"DELETE">>, Key]) ->
    Response = case mcache:delete(Key) of
        not_found -> <<"NOT FOUND">>;
        ok -> <<"DELETED">>
    end,
    {ok, Response};
handle_message([<<"ADD">>, Args]) ->
    [Key, Value] = args(Args),
    Response = case mcache:add(Key, Value) of
        exists -> <<"EXISTS">>;
        ok -> <<"STORED">>
    end,
    {ok, Response};
handle_message([<<"REPLACE">>, Args]) ->
    [Key, Value] = args(Args),
    Response = case mcache:replace(Key, Value) of
        not_found -> <<"NOT FOUND">>;
        ok -> <<"STORED">>
    end,
    {ok, Response};
handle_message([<<"APPEND">>, Args]) ->
    [Key, Value] = args(Args),
    Response = case mcache:append(Key, Value) of
        not_found -> <<"NOT FOUND">>;
        ok -> <<"STORED">>
    end,
    {ok, Response};
handle_message([<<"PREPEND">>, Args]) ->
    [Key, Value] = args(Args),
    Response = case mcache:prepend(Key, Value) of
        not_found -> <<"NOT FOUND">>;
        ok -> <<"STORED">>
    end,
    {ok, Response};
handle_message(_Any) -> {ok, <<"UNKNOWN REQUEST">>}.

args(Args) -> binary:split(Args, <<" ">>).
kv_template({Key, not_found}) -> ["VALUE ", Key, " NOT FOUND\r\n"];
kv_template({Key, Value}) -> ["VALUE ", Key, " ", Value, "\r\n"].

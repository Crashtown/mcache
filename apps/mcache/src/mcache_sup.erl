%%%-------------------------------------------------------------------
%% @doc mcache top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mcache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    McacheSpec = #{id => mcache,
                   start => {mcache, start_link, []},
                   restart => permanent,
                   shutdown => brutal_kill,
                   type => worker,
                   modules => [mcache]},
    McacheServerSpec = #{id => mcache_server,
                         start => {mcache_server, start_link, []},
                         restart => permanent,
                         shutdown => brutal_kill,
                         type => worker,
                         modules => [mcache_server]},
    {ok, {SupFlags, [McacheSpec, McacheServerSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================

-module(conn_handler_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         new_handler/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

new_handler(Sock) ->
    case supervisor:start_child(?SERVER, [Sock]) of
        {ok, Pid} ->
            gen_tcp:controlling_process(Sock, Pid),
            Pid ! run,
            {ok, Pid};
        Error ->
            Error
    end.

init([]) ->
    Child = [{ hcache_net_listener, { hcache_net_listener, start_link, []},
              temporary, 2000, worker, [hcache_net_listener]}],
    {ok, { {simple_one_for_one, 0, 1}, Child} }.

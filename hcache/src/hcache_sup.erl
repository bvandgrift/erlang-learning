-module(hcache_sup).

-behavior(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Cache  = {hcache, {hcache, start_link, []}, permanent, 2000, worker, [hcache]},
    Listen = {hcache_net_listener, {hcache_net_listener, start_link, []}, permanent, 2000, worker, [hcache_net_listener]},

    {ok, {{one_for_one, 10, 60}, [Cache, Listen]}}.

%% get_cache(Sup, ChildIndex) ->
%%     Kids = supervisor:which_children(Sup),
%%     Key  = list_to_atom("hcache" ++ integer_to_list(ChildIndex)),
%%     {Key, Pid, worker, _Modules} = proplists:lookup(Key, Kids),
%%     Pid.

-module(hcache_sup).

-behavior(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1,get_cache/2]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Cache0 = {hcache0, {hcache, start_link, []}, permanent, 2000, worker, [hcache]},
    Cache1 = {hcache1, {hcache, start_link, []}, permanent, 2000, worker, [hcache]},
    Cache2 = {hcache2, {hcache, start_link, []}, permanent, 2000, worker, [hcache]},
    Cache3 = {hcache3, {hcache, start_link, []}, permanent, 2000, worker, [hcache]},

    {ok, {{one_for_one, 10, 60}, [Cache0, Cache1, Cache2, Cache3]}}.

get_cache(Sup, ChildIndex) ->
    Kids = supervisor:which_children(Sup),
    Key  = list_to_atom("hcache" ++ integer_to_list(ChildIndex)),
    {Key, Pid, worker, _Modules} = proplists:lookup(Key, Kids),
    Pid.

-module(hcache).

-behaviour(gen_server).

%% API
-export([start_link/0,
         get/2,
         put/3,
         del/2,
         count/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-define(DEFAULT_TTL, 5000).

-record(state, {cache=dict:new()}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
    %% gen_server:start_link(?MODULE, [], []).

get(Cache, Key) ->
    gen_server:call(Cache, {get, Key}).

put(Cache, Key, Value) ->
    gen_server:cast(Cache, {put, Key, Value}).

del(Cache, Key) ->
    gen_server:cast(Cache, {del, Key}).

count(Cache) ->
    gen_server:call(Cache, count).

init([]) ->
    {ok, #state{}}.

handle_call({get, Key}, _From, #state{cache=Cache}=State) ->
    case dict:is_key(Key, Cache) of
        false ->
            {reply, not_found, State};
        true ->
            {Response, Cache1} = expired_or_value(Key,Cache),        
            {reply, Response, State#state{cache=Cache1}}
            %{reply, cache_expired_or_value(Found), State}
    end;
handle_call(count, _From, #state{cache=Cache}=State) ->
    {reply, dict:size(Cache), State};
handle_call(_Request, _From, State) ->
    error_logger:info_msg("WHUTcall?: ~p~n", [_Request]),
    {reply, ignored, State}.

handle_cast({put, Key, Value}, #state{cache=Cache}=State) ->
    State1 = State#state{cache=dict:store(Key, {Value, erlang:now()}, Cache)},
    {noreply, State1};
handle_cast({del, Key}, #state{cache=Cache}=State) ->
    State1 = State#state{cache=dict:erase(Key, Cache)},
    {noreply, State1};
handle_cast(_Msg, State) ->
    error_logger:info_msg("WHUTcast?: ~p~n", [_Msg]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

expired_or_value(Key,Cache) ->
    {ok, {Value, Time}} = dict:find(Key, Cache),
    Age = timer:now_diff(erlang:now(), Time) div 1000,
    case Age > ?DEFAULT_TTL of
        true -> 
            {expired, dict:erase(Key, Cache)};
        false ->
            {Value, Cache}
    end.

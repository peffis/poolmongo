-module(poolmongo_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, Pools} = application:get_env(poolmongo, pools),
    
    PoolSpecs = lists:map(
                  fun({Name, SizeArgs, WorkerArgs}) ->
                          PoolArgs = [{name, {local, Name}},
                                      {worker_module, mc_worker}] ++ SizeArgs,
                          poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                  end, Pools),

    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.


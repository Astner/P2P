-module(cache_cli).

-export([start/1, init/1, commands/0, prompt/1, cmd/2, error/1]).

start(Cache) ->
    cli:start(?MODULE, Cache).
    

init(Cache) ->
    gsl:print("CLI for host cache up and running."),
    Cache.

error(Cache) ->
    Cache.

commands() ->
    [{i, "Prints a summary of the host cache information."}].

prompt(_Cache) ->
    "HOST CACHE > ".

cmd(i, Cache) ->
    Cache ! print, 
    Cache.


-module(setup).

-export([node/1, nodes/1, system/1, cli/2]).

-include("include.hrl").

cli(Name, Logger) ->
    
    case net_kernel:connect(Logger) of
        true ->
            Pid = whereis(Name, 10, 10),
            node_cli:start(Pid, Name);
        false  ->
            %% TODO-KM: Change to report_error!!
            ?INFO([{Name, not_registered}]),
            exit({error, loggerl})
    end.
    
  

whereis(Name, Sleep, Delta) ->
    io:format("Looking up ~w: ", [Name]),
    whereis_loop(Name, Sleep, Delta).

whereis_loop(Name, Sleep, Delta) ->
    case global:whereis_name(Name) of
        undefined ->
            io:format(". "),
            timer:sleep(Sleep),
            whereis_loop(Name, Sleep+Delta, 2*Delta);
        Pid  ->
            io:format("Pid is ~w.~n", [Pid]),
            Pid
        end.

node(Name) ->
    Data = data(Name),
    node:start(Name, Data). 
   
nodes(Names) ->
    lists:foreach(fun node/1, Names).

%% First start the host cache, then starts multiple peers.
system(NodeNames) ->
    cache:start(),
    ?MODULE:nodes(NodeNames).


data(Name) ->
    %% Data for a few peer names.     
     Map = [{north, "Scandinavia.txt"}, 
	   {east, "Asia.txt"},
	   {west, "South_America.txt"},
	   {south, "Africa.txt"}],
    
    case lists:keyfind(Name, 1, Map) of
        {Name, FileName} ->
            %% Load data from file. 
            Path = "data/" ++ FileName,
            load(Path);
        false ->
            %% No data availabl. 
            []
    end.

load(Name) ->
    {ok, Device} = file:open(Name, [read]),
    load(Device, []).

load(Device, Accum) ->
    case io:get_line(Device, "") of
        eof  -> 
	    file:close(Device), 
	    Accum;
	"\n" -> 
	    load(Device, Accum);
        Line ->
	    Country  = re:replace(Line, "\n", "", [{return, list}]),
	    Capital  = re:replace(io:get_line(Device, ""), "\n", "", [{return, list}]),
	    
	    load(Device, [{Country, Capital}|Accum])
    end.

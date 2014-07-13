-module(utils).

-export([start_node/3, whereis/3, start_peer/2, start_peer/3, sort_peers_by_items/1]).

-include("include.hrl").

sort_peers_by_items(Peers) ->
    lists:sort(fun(A, B) -> A#peer.items > B#peer.items end, Peers).

%% @doc THIS IS PROPABLY THE SHIT!!!                                                           
-spec start_node(Name, NameType, Options) -> {ok, pid()} | {error, Reason} when                
      Name     :: atom(),
                                                                            
      NameType :: shortnames | longnames,
                                                            
      Options  :: [Option],
                                                                          
      Option   :: {cookie, atom()},
                                                                  
      Reason   :: {already_started, pid()} | term().                                           
                                                                                               
start_node(Name, NameType, Options) ->
    
    case net_kernel:start([Name, NameType]) of                                                 
        {ok, _PID} ->
            {ok, options, Options};
        X  ->
            io:format("ERROR: ~p", [X]),
            X;
        {error, Error} -> 
            print_error(Name, NameType),
            exit({error, node_already_started, Error})
        
    end.                                                                                       


print_error(Name, NameType) ->
    HostName = net_adm:localhost(),
    
    PrintHostName = case NameType of
                    shortnames ->
                        [H|_] = string:tokens(HostName, "."),
                        H;
                    longnames ->
                        HostName
                end,
    
    PrintNodeName = io_lib:format("~s@~s", [Name, PrintHostName]),
    
    {ok, Cols}  = io:columns(),
    
    BigLine = io_lib:format("~*..*s", [Cols, $=, ""]),

    MsgStr = io_lib:format("Node ~s already started!", [PrintNodeName]),

    MsgStrCentre = string:centre(MsgStr, Cols),
    
    io:format("~n~n~s~n~s~n~s~n~n", [BigLine, MsgStrCentre, BigLine]).



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
               


%% @doc Start a named peer.

%% TODO-KM: Can we do without the GSL argument and simply rely on the
%% GSL to be globally registered under name gls? - No, don't think so
%% ... cannot use global:whereis_name/1 if calling node not connected
%% to the node where gsl is running.
start_peer(Name, Data, GSL) ->
    utils:start_node(Name, shortnames, []),
    
    case net_kernel:connect(GSL) of
        true ->
            Pid = whereis(gsl, 10, 10),
            gsl:print("Global System Logger (GSL) found with Pid ~w", [Pid]);
        false  ->
            io:format("Global System Logger (GSL) not found alive :-(~n"),
            exit({error, no_gsl})
    end,
    
    Self = peer:start(Name, Data),
    N = length(Data),
    gsl:print("Data (~w pairs) loaded for peer ~w", [N, Name]),
    peer_cli:start(Self, Name).
    

start_peer(Name, GSL) ->
    Data = load_peer_data(Name),
    start_peer(Name, Data, GSL).
        
load_peer_data(Name) ->
     Map = [{north, "Scandinavia.txt"}, 
	   {east, "Asia.txt"},
	   {west, "South_America.txt"},
	   {south, "Africa.txt"}],
    
    %% {value,{_, FileName}} = 
    case lists:keyfind(Name, 1, Map) of
        {Name, FileName} ->
            
            Path = "data/" ++ FileName,
            io:format("Path = ~s~n", [Path]),
            
            data:load(Path);
        false ->
            []
    end.
    
        

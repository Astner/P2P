-module(node_cli).
-include("include.hrl").

-export([start/2, init/1, commands/0, prompt/1, cmd/3, error/1]).


start(Pid, Name) ->
    gen_cli:start(?MODULE, {Pid, Name, 0}).
    

init({Self, Name, N}) ->
    ?INFO(["CLI up and running", {node, Name}]),
    
    %% A small sleep, Otherwiese the above infor report most likely apperas
    %% before the below new lines.
    timer:sleep(100), 
    io:format("~n~n~n"),
    
    {Self, Name, N}.

error(State) ->
    State.

commands() ->
    [{info, [i],"Info about this peer."},
     {ping, [p],"Send ping to known peers."},
     {register, [r],"Register at the host cache"}, 
     {add,[a], "Add data"}, 
     {search, [s], "Search by country"}].

prompt({Self, Name, _N}) ->
    NewN = node:stats(Self),
    io_lib:format("~s (~w) $> ", [string:to_upper(atom_to_list(Name)), NewN]).

strip_input(String) ->
    string:strip(string:strip(String, right, $\n), both).

get_non_empty_line(Prompt) ->
    Line = strip_input(io:get_line(Prompt)),
    
    case Line of
        "" ->
            get_non_empty_line(Prompt);
        _ -> 
            Line
    end.
    
search_peers(Query, State) ->
    do_cmd({search, peers, Query}, State).
    
cmd(search, Query, {_Self, _, _}= State) ->
    %% Search both localy 
    {_NewState, Result} = do_scmd({search, local, Query}, State),
    
    %% And among all known peers. 
    _NewState = search_peers(Query, State),
    
    case Result of 
        [] -> io:format("No local match found.~n");
        R ->  io:format("Local match ~s found.~n", [R])
    end,
    State;

cmd(info, _, State) ->
    {NewState, Result} = do_scmd(info, State),
    DL =  [{"Country", "Capital"}, 
             {line, $=}] ++ 
        Result#pstate.data,

    PL =  ["", {"Peer", "Items"}, {line, $=}] ++ 
        case Result#pstate.peers of 
            [] ->
                ["none"];
            Peers ->
                [{atom_to_list(P#peer.name), integer_to_list(P#peer.items)} || P <- Peers]
        end,
     
    Format = fun({Q, {H, M, S}, Matches}) ->
                    %% http://noss.github.io/2009/03/29/erlang-io-format-to-string.html
                    Raw = io_lib:format('~2..0b:~2..0b:~2..0b ~p', [H, M, S, Matches]),
                    Str = erlang:binary_to_list(erlang:iolist_to_binary(Raw)),
                    {Q, Str};
               (Any) ->
                    error_logger:info_report([{any, Any}])
            end,
    
    HL = ["", {"Remote query", "Match"}, {line, $=}] ++ 
        case Result#pstate.history of 
            [] ->
                ["none"];
            History -> 
                [Format(H) || H <-  History]
        end,
    table:print(DL ++ PL ++ HL),
             
    NewState;

cmd(register, _, State) ->
    do_cmd(register, State);
cmd(ping,_,  State) ->
    do_cmd(ping, State);
cmd(add, Args, State) ->
    Country = get_non_empty_line("    Country: "),
    Capital = get_non_empty_line("    Capital: "),
    
    do_cmd({add, Country, Capital}, State).

do_cmd(Cmd, {Node,_, _} = State) ->
    Node ! {cmd, Cmd},
    State.
    

%% Synchronous command. 

do_scmd(Cmd, {Node, _, _} = State) ->
    Node ! {cmd, Cmd, self()},
    receive
        {result, Cmd, Result} ->
            Result
    end,
     
    {State, Result}.



%%  Peer to peer computing (1DT047) 2014
%%
%%  Lab exercise 2
%%
%%  Solution by: <your name here>

-module(node).
-export([start/2, stats/1]).

-include("include.hrl").

%% @Start a peer with Name and Data.
start(Name, Data) ->
    Pid = spawn(fun() -> loop(#pstate{name=Name, data=Data}) end),
    yes = global:register_name(Name, Pid),

    ?INFO(["New Smutella node spawned and registered.", 
           {node, node()}, {name, Name}, {pid, Pid}]),
    Pid.

%% @doc A Snutella peer, also refered to as a Snutella node keeps an
%% internal `State'. The node waits for messages from the command line
%% interface (CLI), the Snutella host cache or other peers in the
%% Snutella peer-to-peer network.
%% 
%% When a message is received and matched, proper action is taken and
%% a new state is constructed and the

loop(State) ->

    receive
        
        %% == CLI Actions ==================================
        %%    Messages received as result of CLI commands. 
        %%==================================================
        
        %% Asynchronous commands. 
        
	{cmd, Cmd} ->
            
            NewState = handle_cmd(Cmd, State),
            loop(NewState);

        %% Synchronous CLI commands where the CLI blocks until
        %% receiving a reply.

	{cmd, Cmd, From} ->
            NewState = handle_scmd(Cmd, From, State),
            loop(NewState);
        
        %% == Host cache Actions ===========================
        %%    Messages sent by the host cache. 
        %% =================================================
        
        {welcome, no_peers} ->
            loop(State);
        
        {welcome, Peer} -> 
            NewState = update_peers(State, Peer),
            loop(NewState);
        
        %% == Peer Actions ================================
        %%    Messages sent by other peers
        %% ================================================
        
        {action, Action} ->
            NewState = do_peer_action(Action, State),
            loop(NewState);
        
        {action, Action, from, Peer} ->
            NewState = handle_peer_action(Action, from, Peer, State),
            loop(NewState);

        %% == Orserver Actions ============================
        %%    Used by the CLI to update the prompt with the 
        %%    number of known peers. 
        %% ================================================

        {stats, From} ->
            From ! {ok, {stats, length(State#pstate.peers)}},
            loop(State);
                
        %% == Catch all pattern ===========================
        
        Any ->
	    ?WARNING(["Un handled message received", {message, Any}]),
	    loop(State)
                
    end.


%% TODO 1 

%% @doc This function should return the new state after a new data
%% pair have been added.
%% 
%% State is a record. Use the record syntax to construct a new state
%% record based on the old one.

handle_cmd({add, Country, Capital}, State) ->
    NewData = [{Country, Capital} | State#pstate.data],
    NewState = State#pstate{data=NewData}; 
    %% TODO: you must change this. 



%% TODO 2 

handle_cmd(register, State) ->
    
    %% Look up the PID of the host cache. 
    Cache =  utils:whereis(cache, 10, 10),
    
    %% The cache only needs to know the Pid and amount of data. This
    %% data is kept in a peer record which makes it easy to add or
    %% change in the future.
    
    Self = mk_peer(State),
    
    %% TODO: Here you must send the tuple {hello, from, Self} to the host cache. 
    Cache ! {hello, from, Self},
    
    %% Nothing have changed, return the State unchanged.
    State;  

%% TODO 2 

handle_cmd(ping, State) ->
    ping(State), %% Pings are sent by seprate function. 
    State;


%% Asynchronous searching all known peers. The CLI will not block
%% waiting for responses. Responses, if any, will be sent to the
%% background peer procces. When a repsonse comes back, the history
%% field of the peer state record is updatet. This update can be seen
%% from the CLI by issuing the info command. 
handle_cmd({search, peers, Query}, State) ->
    
    do_action_all_peers({search, Query}, State), 

    State;



%% Decrepated?
handle_cmd(info, State) ->
    ?INFO([{state, State}, {foo, bar}]),
    
    State.


handle_peer_action({search, Query}, from, Peer, State) ->
    
    case search_data(State, Query) of
        [] ->
            ignore;
        Matches ->
            Peer#peer.pid ! {action, {search, Query, matches, Matches}}
    end,
    State.


update_peers(State, Buddy) ->
    %% Tuples are records.  #Record.field equals the index of field in
    %% the underlying typle.

    %% All known peers
    Peers = State#pstate.peers,
    
    %% Update the Buddy peer if alreay known, otherwise insert the peer. 
    NewPeers = lists:keystore(Buddy#peer.pid, #peer.pid, Peers, Buddy),
    
    

    %% TODO: You must update and return the new state.
    State#pstate{peers=NewPeers}.
    
    

%% TODO-KM: Really need to refactor using a record as loop state.



search_data(State, Query) ->
    %% TIP: Check out the lists:keysearch/3 function in the Erlang
    %% standard library.
    
    DataList = State#pstate.data,
    
    %%false is returned of Query is not found, otherwse the key,value tuple is returned
    Answer = lists:keysearch(Query, 1, DataList),
    
    if 
	Answer == false ->
	    [];
	true ->
	    [Answer]
    end. %% TODO: You must update this. 

%% END SKIP LINES


%% Synchrounous CLI commands that waits for data to be sent back.

handle_scmd(info, From, State) ->
    From ! {result, info, State}, 
    State;


%% A local peer search from the CLI.
handle_scmd({search, local, Query}, From, State) ->
    
    
        
    Reply = search_data(State, Query),

        
    %% Send result back to CLI. 
    From ! {result, {search, local, Query}, Reply},
    
    %% No change to state. 
    State.
   


%% Sends an action tuple to all known peers. 
do_action_all_peers(Action, State) ->
    Self = mk_peer(State), 
    Msg = {action, Action, from, Self},
    
    lists:foreach(fun(Peer) -> Peer#peer.pid ! Msg end, State#pstate.peers), 
    
    State.



table(State) ->
    H1 = {"Country", "Capital"},
    H2 = "Known peers",
        
    Data = [io_lib:format("~s : ~s", [Country, Capital]) || {Country, Capital} <- State#pstate.data],
    
    %% DataStr = io_lib:format(string:join(Data, "~n"), []),
    
    Peers =   [io_lib:format("~w : ~w", [Peer#peer.name, Peer#peer.items]) || Peer <- State#pstate.peers],
    %% PeersStr = io_lib:format(string:join(Peers, "~n"), []),
    
    Lines = [H1] ++ Data ++ [H2] ++ Peers,
    
    {ok, Cols} = io:columns(),
    
    CentredLines =  [string:centre(S, Cols) || S <- Lines],
    
    string:join(CentredLines, "~n").



    
stats(PeerPid) ->
    PeerPid ! {stats, self()},
    receive
        {ok, {stats, Stats}} ->
            Stats
    end.
        
mk_peer(State) ->
    #peer{pid=self(),
          name=State#pstate.name,
          items=length(State#pstate.data)
         }.

do_peer_action({ping, from, Peer}, State) ->
    
    ?INFO([{ping_from, Peer}]),
    
    %% SEND PONG TO PEER HERE
    
    Peer#peer.pid ! {action, {pong, from, mk_peer(State)}},

    State;

do_peer_action({pong, from, Peer}, State) ->
    
    ?INFO([{pong_from, Peer}]),
    
    %% NewState = State; %% YOU MUST CHANGE THIS
    update_peers(State, Peer); 

do_peer_action({search, Query, matches, Matches}, State) ->
    
    %% Time stamp the event.
    MatchEvent = [{Query, time(), Matches}],
    
    %% Append the event to the history
    NewHistory = State#pstate.history ++ [{Query,time(), Matches}],
    
    %% TODO: You must update the history field of the State record and
    %% return this as the new state.
    
    State.
    



%%  TODO 3


ping(State) ->
    ping(State, State#pstate.peers).

ping(_, []) ->
    done;

ping(State,  [ Peer | Peers]) ->
    
    %% Construct a ping message
    
    Self = mk_peer(State),
    Msg = {action, {ping, from, Self}},
    
    %% TODO: You must send Msg to the peer. The Pid of the peer is
    %% part of the Peer#peer record. 

    PeerPID = Peer#peer.pid,
    PeerPID ! Msg,
    

    
    ping(State, Peers).


    

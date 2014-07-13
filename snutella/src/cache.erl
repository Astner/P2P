-module(cache).
-export([start/0, stop/0]).
-include("include.hrl").

start() ->
    
    CachePid = spawn(fun() -> loop([]) end),
    global:register_name(cache, CachePid),
    
    ?INFO(["Snutella host cache process spawned and registered.", 
           {node, node()}, {name, cache}, {pid, CachePid}]), 
    
    CachePid.

stop() ->
    %% Unregister cache?
    
    net_kernel:stop().

loop(Peers) ->
    receive
	print ->
	    io:format("Peers: ~p~n~n", [Peers]),
	    loop(Peers);
        {hello, random, _N, _Peer} ->
            %% Send back N random peers.
            tbi;
        
	{hello, from, Peer} ->
	    ?INFO([{hello_from, Peer}]),
	    
	    %% Make sure the calling Peer is not returned itself
	    
	    SelectFrom = [ P  || P <- Peers, P#peer.pid =/= Peer#peer.pid],
            
	    case (SelectFrom) of
		[] ->
                    %% No other peers available :-(
		    NewPeers = [Peer],
		    Peer#peer.pid ! {welcome, no_peers};
		
		_ ->
		    %% At least one more peer available :-D
                    
                    %% Select one and send to Peer.
                    
		    [Buddy | Others ] = SelectFrom,
                    ?INFO([{welcome, Peer}, {buddy, Buddy}]),
                    
                    Peer#peer.pid ! {welcome, Buddy},
                    
                    %% Don't forgett to add the new Peer to the list
                    %% of know peers.
                    
		    NewPeers = [Peer | Others] ++ [Buddy]
	    end,
	    
            ?INFO([{peers, NewPeers}]),
            
            loop(NewPeers)
    end.


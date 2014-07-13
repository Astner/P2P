-module(simple). 

-export([start/2]).

-record(state, {number=0, list=[], requestsMade = 0, maxRequests = 10}).
	

start(v1, R) ->
    spawn(fun() -> loop(v1, {0, []}, 0, R) end);
start(v2, R) ->
    spawn(fun() -> loop(v2, #state{maxRequests = R}) end).

exitWithPrint(v1, State) ->
    io:format("Exits v1 with state: ~w~n", [State]);
exitWithPrint(v2, State) ->
    io:format("Exits v2 with state: ~w~n", [State]).

                  
loop(v1, {N, L} = State, RequestsMade, MaxRequests) ->
    receive 
        {add_number, M} ->
	    if 
		RequestsMade + 1 < MaxRequests ->
		    loop(v1, { N + M, L}, RequestsMade + 1, MaxRequests);
		true -> 
		    %% Equals or greather than
		    exitWithPrint(v1, { N + M, L})
	    end;

        {add_to_head, X} ->
	    if 
		RequestsMade + 1 < MaxRequests ->		    
		    loop(v1, {N, [ X | L ]}, RequestsMade + 1, MaxRequests);
		true -> 
		    %% Equals or greather than
		    exitWithPrint(v1, {N, [ X | L ]})
	    end;

        print ->
	    %% I choose to not count the print operation as a request, 
	    %% only operations affecting the state are counted
            io:format("State = ~w~n", [State]),
            loop(v1, State, RequestsMade, MaxRequests);
        Any ->
	    %% I choose not to count invalid requets to the number of requests made
            io:format("Unknown message ~w~n", [Any]),
            loop(v1, State, RequestsMade, MaxRequests)
    end.

loop(v2, State) ->
    receive 
        {add_number, M} ->
            NewNumber = State#state.number + M,
	    NewRequests = State#state.requestsMade + 1,
 
	    if
		NewRequests < State#state.maxRequests ->
		    loop(v2, State#state{number = NewNumber, requestsMade = NewRequests});
		true ->
		    exitWithPrint(v2, State#state{number = NewNumber, requestsMade = NewRequests})
	    end;
        {add_to_head, X} ->
            NewList = [ X | State#state.list ],
	    NewRequests = State#state.requestsMade + 1,

	    if
		NewRequests < State#state.maxRequests ->
		    loop(v2, State#state{list=NewList, requestsMade = NewRequests});
		true ->
		    exitWithPrint(v2, State#state{list=NewList, requestsMade = NewRequests})
	    end;

        print ->
            io:format("State = ~w~n", [State]),
            loop(v2, State#state{});
        Any ->
            io:format("Unknown message ~w~n", [Any]),
            loop(v2, State#state{})
    end.
                 

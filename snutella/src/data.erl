-module(data).

-export([load/1]).

%% http://www.trapexit.org/Reading_Lines_from_a_File

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

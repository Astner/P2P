-module(table).

-compile(export_all).

data() ->
    ["Country : Capital", 
     {line, $=},
     {"aaa", "bbbbbbbbb"},
     {"cccccccccc", "ddd"}, 
     "",
     "Peer : Itemss",
     {line, $=},
     {"apa", "13"},
     {"kossa", "7"}].


foo({A,B}) ->
    {tuple, A, B, max(length(A), length(B))};
foo(A) ->
    {string, A, length(A)}.

pad({line, Char}, Width) ->
    Str = io_lib:format("~c", [Char]),
    string:copies(Str, 2*Width +3);
pad({Left, Right}, Width) ->
    string:right(Left, Width, $ ) ++ " : " ++ string:left(Right, Width, $ );
pad(Line, Width) ->
    string:centre(Line, 2*Width + 3, $ ).
    

tabularize(Data) ->
    Pairs = [ P || P = {_, _} <- Data],
    Max  =  lists:max([  max(length(Left),length(Right))  || {Left,Right} <- Pairs, Left =/= line]),
     
    MaxOdd = case Max rem 2 of
                 0 ->
                     Max;
                 _ -> Max +1
             end,
    
    [ pad(D, MaxOdd) || D <- Data ].
    

print(Data) ->
    %% NOTE: Might break if Data contians "~".
    {ok, Cols} = io:columns(),
    Lines = [string:centre(Line, Cols-1, $ ) || Line <- tabularize(Data)],
    
    io:format(string:join(Lines, "~n") ++ "~n", []).
    
    %% lists:foreach( fun(L) -> io:format("~s~n", [L]) end, Lines ).



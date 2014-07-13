-module(gen_cli).


-compile(export_all).

start(Mod, Args) ->
    
    loop(Mod, Mod:init(Args)).

loop(Mod, State) ->

    {ok, RegExp} = re:compile("^\\s*(\\S+)(.*)$"),

    Prompt = Mod:prompt(State),
    
    Line1 = string:strip(io:get_line(Prompt), right, $\n),
    Line2 =  string:strip(Line1, both),
    
    case Line2 of 
        "" ->
            loop(Mod, State);
        _ -> 
            
            %% Cmd = list_to_atom(Line2),
            
            {match, [CmdString, ArgString]} =  re:run(Line2, RegExp, [{capture, [1,2], list}]),
            
            ArgString2 = string:strip(ArgString, both),

            Cmd = list_to_atom(CmdString),
            case Cmd of
                q ->
                    quit;
                h ->
                    io:format("~n" ++ help(Mod) ++ "~n~n"),
                    loop(Mod, State);
                
                _ ->
                    case is_valid(Mod, Cmd) of
                        {ok, Canonical} ->
                            loop(Mod, Mod:cmd(Canonical, ArgString2, State));
                        {error, Cmd} ->
                            io:format("~nInvalid command ~w. ~s~n~n", [Cmd,  io_lib:format(help(Mod), [])]),
                            loop(Mod, Mod:error(State))
                    end
                        
            end
                
    end.


is_valid(Mod, Cmd) ->
    
    F = fun({Long, Shorts, _Description}) ->
                [{Long, Long} |  [ {Short, Long} || Short <- Shorts] ]end,
    
    M = lists:concat([ F(Tuple) || Tuple <- Mod:commands()]),
     
    case lists:keyfind(Cmd, 1, M) of
        {_,Canonical} ->
            {ok, Canonical};
        false  ->
            {error, Cmd}
    end.



%% TODO: Move all stuff related to formating the command table to a
%% separate module.

indent(String, N, PadChar) ->
    Pad = [ PadChar || _ <- lists:seq(1, N)],
    Pad ++ String.
    
split_list(N, L) ->
    split_list(N, L, []).

split_list(N, L, LS) when length(L) =<  N ->
    lists:reverse([L|LS]);
split_list(N, L, LS) ->
    {A, B} = lists:split(N, L),
    split_list(N, B, [A|LS]).
    
indent_to(C1W, C2W, String, Caption) ->
    [Line | Lines] = split_list(C2W, String),
    
    L1 = case Caption of 
             "" ->
                 io_lib:format("~*.. s   ~s", [C1W, "", Line ]);
             _ ->
                 io_lib:format("~*.. s : ~s", [C1W, Caption, Line ])
         end,
             
    Ls = string:join([io_lib:format("~*.. s   ~s", [C1W, "", string:strip(S, left)]) || S <- Lines], "~n"),
    
    NL = case Ls of 
             "" -> "";
             _ -> "~n"
         end,
    
    L1 ++ Ls ++ NL.

format_help_line(C1W, C2W, {Canonical, Aliases, DocString}) ->
    
    A = case Aliases of 
            [] -> "";
            _ ->
                AliasString = "Aliases: " ++ string:join([atom_to_list(Alias) || Alias <- Aliases], ", ") ++ ".",
                "~n" ++ indent_to(C1W, C2W, AliasString, "")
        end,
   
    AliasesStrings = [atom_to_list(Alias) || Alias <- Aliases],
    
    Right = string:join([atom_to_list(Canonical)] ++ AliasesStrings, "/"),

    indent_to(C1W, C2W, DocString, Right).

format_help_lines(Mod) ->
    C1W = lists:max([length(atom_to_list(element(1,T))) || T <- Mod:commands()]) + 4,
    {ok, Cols} = io:columns(),
    C2W = Cols - C1W -5,
    
    string:join([format_help_line(C1W, C2W, T) || T <- Mod:commands()], "~n").

                 
        
help(Mod) ->
    H = io_lib:format("Available commands:~n~n",[]),
    H ++  format_help_lines(Mod).

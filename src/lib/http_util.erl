-module(http_util).

-export([parse_headers/2, check_headers/2]).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("ts_websocket.hrl").

%%--------------------------------------------------------------------
%% Func: parse_headers/3
%% Purpose: Parse HTTP headers line by line
%% Returns: {ok, #http, Body}
%%--------------------------------------------------------------------
parse_headers(Http, Tail) ->
    case get_line(Tail) of
        {line, Line, Tail2} ->
            parse_headers(parse_line(Line, Http), Tail2);
        {lastline, Line, _} ->
            {ok, parse_line(Line, Http)}
    end.

check_headers(Headers, RequiredHeaders) ->
    F = fun({Tag, Val}) ->
                Term = if
                           is_atom (Tag) -> Tag;
                           true -> string:to_lower(Tag)
                       end,
                %% see if the required Tag is in the Headers
                case proplists:get_value(Term, Headers) of
                    %% header not found, keep in list
                    false -> true;
                    HVal ->
                        case Val of
                            %% ignore value -> ok, remove from list
                            ignore -> false;
                            %% expected val -> ok, remove from list
                            HVal -> false;
                            %% val is different, keep in list
                            _ -> true
                        end		
                end
        end,

    case lists:filter(F, RequiredHeaders) of
        [] -> true;
        MissingHeaders -> MissingHeaders
    end.

%%--------------------------------------------------------------------
%% Func: parse_status/2
%% Purpose: Parse HTTP status
%% Returns: #http
%%--------------------------------------------------------------------
parse_status([A,B,C|_], Http) ->
    Status=list_to_integer([A,B,C]),
    Http#http{status = Status}.

%%--------------------------------------------------------------------
%% Func: parse_line/3
%% Purpose: Parse a HTTP header
%% Returns: #http
%%--------------------------------------------------------------------
parse_line("http/1.1 " ++ TailLine, Http)->
    parse_status(TailLine, Http);
parse_line("http/1.0 " ++ TailLine, Http)->
    parse_status(TailLine, Http#http{close=true});

parse_line("upgrade: " ++ TailLine, Http) ->
    Headers = [{'Upgrade', TailLine} | Http#http.headers],  
    Http#http{headers=Headers};
parse_line("connection: " ++ TailLine, Http) ->
    Headers = [{'Connection', TailLine} | Http#http.headers],
    Http#http{headers=Headers};
parse_line("sec-websocket-accept: " ++ TailLine, Http) ->
    Headers = [{'Sec-WebSocket-Accept', TailLine} | 
               Http#http.headers],
    Http#http{headers=Headers, accept=TailLine};
parse_line(_Line, Http) ->
    Http.

%% code taken from yaws
is_nb_space(X) ->
   lists:member(X, [$\s, $\t]).

%% ret: {line, Line, Trail} | {lastline, Line, Trail}
get_line(L) ->
    get_line(L, true, []).

get_line("\r\n\r\n" ++ Tail, _Cap, Cur) ->
    {lastline, lists:reverse(Cur), Tail};
get_line("\r\n", _, _) ->
    {more};
get_line("\r\n" ++ Tail, Cap, Cur) ->
    case is_nb_space(hd(Tail)) of
        true ->  %% multiline ... continue
            get_line(Tail, Cap,[$\n, $\r | Cur]);
        false ->
            {line, lists:reverse(Cur), Tail}
    end;
get_line([$:|T], true, Cur) -> % ':' separator
    get_line(T, false, [$:|Cur]);%the rest of the header isn't set to lower char
get_line([H|T], false, Cur) ->
    get_line(T, false, [H|Cur]);
get_line([Char|T], true, Cur) when Char >= $A, Char =< $Z ->
    get_line(T, true, [Char + 32|Cur]);
get_line([H|T], true, Cur) ->
    get_line(T, true, [H|Cur]);
get_line([], _, _) -> %% Headers are fragmented ... We need more data
    {more}.

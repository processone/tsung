%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%% File    : ts_search.erl
%%% Author  : Mickael Remond <mickael.remond@erlang-fr.org>
%%% Description : Add dynamic / Differenciated parameters in Tsunami
%%%               request and response
%%%               The function subst is intended to be called for each
%%%               relevant field in ts_protocol implementation.
%%%               TODO: Insert exemple
%%% Created : 22 Mar 2004 by Mickael Remond <mickael.remond@erlang-fr.org>

-module(ts_search).
-vc('$Id$ ').

-export([subst/2, match/2, parse_dynvar/2]).

-include("ts_profile.hrl").

%% ----------------------------------------------------------------------
%% Function: subst/2
%% Purpose: search into a given string and replace %%Mod:Fun%% strings
%%          by the result of the call to Mod:Fun(Pid) where Pid is the
%%          Pid of the client The substitution tag are intended to
%%          be used in idx-tsunami.xml scenarii files.
%% ----------------------------------------------------------------------
subst(Atom, DynVar) when atom(Atom) ->
    Atom;
subst(Binary, DynVar) when binary(Binary) ->
    list_to_binary(subst(binary_to_list(Binary), DynVar));
subst(String, DynVar) ->
    subst(String, DynVar, []).

subst([], DynVar, Acc) ->
    lists:reverse(Acc);
subst([$%,$%,$_|Rest], DynVar, Acc) ->
    extract_variable(Rest, DynVar, Acc, []);
subst([$%,$%|Rest], DynVar, Acc) ->
    extract_module(Rest, Acc, []);
subst([H|Tail], DynVar, Acc) ->
    subst(Tail, DynVar, [H|Acc]).

%% Search for the module string in the subst markup
extract_module([],Acc,Mod) ->
    lists:reverse(Acc);
extract_module([$:|Tail],Acc,Mod) ->
    extract_function(Tail,Acc,lists:reverse(Mod),[]);
extract_module([H|Tail],Acc,Mod) ->
    extract_module(Tail,Acc,[H|Mod]).

%% Search for the module string in the subst markup
extract_variable([],DynVar,Acc,Var) ->
    lists:reverse(Acc);
extract_variable([$%,$%|Tail], undefined, Acc, Var) ->
    subst(Tail, undefined, lists:reverse("undefined") ++ Acc);
extract_variable([$%,$%|Tail], DynVar, Acc, Var) ->
    VarName = list_to_atom(lists:reverse(Var)),
    case lists:keysearch(VarName,1,DynVar) of 
        {value, {VarName, Result}} ->
            subst(Tail, DynVar,lists:reverse(Result) ++ Acc);
        _ ->
            ?LOGF("DynVar: no value found for var ~p~n",[VarName],?WARN),
            subst(Tail, DynVar,lists:reverse("undefined") ++ Acc)
    end;
            
extract_variable([H|Tail],DynVar,Acc,Mod) ->
    extract_variable(Tail,DynVar,Acc,[H|Mod]).

%% Search for the function string and do the real substitution before
%% keeping on the parsing
extract_function([], Acc, Mod, Fun) ->
    lists:reverse(Acc);    
extract_function([$%,$%|Tail], Acc, Mod, Fun) ->
    Module = list_to_atom(Mod),
    Function = list_to_atom(lists:reverse(Fun)),
    Result = Module:Function(self()),
    subst(Tail, lists:reverse(Result) ++ Acc);
extract_function([H|Tail], Acc, Mod, Fun) ->
    extract_function(Tail, Acc, Mod, [H|Fun]).

%%----------------------------------------------------------------------
%% Func: match/2 
%% Args: 
%% Purpose: 
%%----------------------------------------------------------------------
match(undefined, Data) -> ok;
match(RegExp, Data)  when is_binary(Data)->
    ?DebugF("Matching Data size ~p~n",[size(Data)]),
    match(RegExp, binary_to_list(Data));
match(RegExp, String) ->
    case regexp:first_match(String, RegExp) of
        {match,Start,Length} ->
            ?LOGF("Ok Match (regexp=~p) ~n",[RegExp], ?INFO),
            ts_mon:add({count, match}),
            ok;
        nomatch ->
            ?LOGF("Bad Match (regexp=~p), ~n",[RegExp], ?NOTICE),
            ts_mon:add({count, nomatch});
        {error,Error} ->
            ?LOGF("Error while matching: bad REGEXP (~p)~n", [RegExp], ?WARN),
            ts_mon:add({count, badregexp})
    end.

%%----------------------------------------------------------------------
%% Func: parse_dynvar/2 
%% Args: 
%% Purpose: 
%%----------------------------------------------------------------------
parse_dynvar(undefined, Data) -> [];
parse_dynvar(DynVar, Data)  when is_binary(Data) ->
    String = binary_to_list(Data),
    ?DebugF("Parsing Dyn Variable; data is ~p~n",[String]),
    parse_dynvar(DynVar, String,[]);
parse_dynvar(DynVar, Data)  ->
    ?LOGF("Error while Parsing dyn Variable(~p) ~p~n",[DynVar],?WARN),
    [].
    

parse_dynvar([], String, ValuesList) -> ValuesList;
parse_dynvar([{VarName, RegExp}| DynVars], String, ValuesList) ->
    case gregexp:groups(String, RegExp) of
        {match,[Value| Rest]} ->
            ?LOGF("Ok Match (~p=~p) ~n",[VarName, Value], ?DEB),
            parse_dynvar(DynVars, String, [{VarName, Value}| ValuesList]);
        nomatch ->
            ?LOGF("Dyn Var: no Match (varname=~p), ~n",[VarName], ?WARN),
            parse_dynvar(DynVars, String, ValuesList);
        {error,Error} ->
            ?LOGF("Error while parsing dyn var: bad REGEXP (~p)~n", [Error], ?ERR),
            parse_dynvar(DynVars, String, ValuesList)
    end;
parse_dynvar(Args, String, Values) ->
    ?LOGF("Bad args while parsing dyn var (~p)~n", [Args], ?ERR),
    [].
    

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

-export([subst/1]).

%% ----------------------------------------------------------------------
%% Function: subst/1
%% Purpose: search into a given string and replace %%Mod:Fun%% strings
%%          by the result of the call to Mod:Fun(Pid) where Pid is the
%%          Pid of the client The substitution tag are intended to
%%          be used in idx-tsunami.xml scenarii files.
%% ----------------------------------------------------------------------
subst(Atom) when atom(Atom) ->
    Atom;
subst(Binary) when binary(Binary) ->
    list_to_binary(subst(binary_to_list(Binary)));
subst(String) ->
    subst(String, []).

subst([], Acc) ->
    lists:reverse(Acc);
subst([$%,$%|Rest], Acc) ->
    extract_module(Rest, Acc, []);
subst([H|Tail], Acc) ->
    subst(Tail, [H|Acc]).

%% Search for the module string in the subst markup
extract_module([],Acc,Mod) ->
    lists:reverse(Acc);
extract_module([$:|Tail],Acc,Mod) ->
    extract_function(Tail,Acc,lists:reverse(Mod),[]);
extract_module([H|Tail],Acc,Mod) ->
    extract_module(Tail,Acc,[H|Mod]).

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

%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2001 IDEALX
%%%
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

-module(ts_utils).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-include("../include/ts_profile.hrl").

%% user interface
-export([debug/3, debug/4, get_val/1, init_seed/0, chop/1, elapsed/2,
		now_sec/0]).

get_val(Var) ->
	case application:get_env(Var) of 
		{ok, Val} ->
			Val;
		_ ->
			undef_var
	end.

%%
debug(From, Message, Level) ->
	debug(From, Message, [], Level).

debug(From, Message, Args, Level) ->
	Debug_level = ?debug_level,
	if 
		Level =< Debug_level ->
			error_logger:info_msg("~20s:(~p) "++ Message,
					  [From, Level] ++ Args);
		true ->
			nodebug
	end.

%% print elapsed time in microseconds
elapsed({Before1, Before2, Before3}, {After1, After2, After3}) ->
    After  = After1  * 1000000000000  + After2  * 1000000 + After3,
    Before = Before1 * 1000000000000  + Before2 * 1000000 + Before3,
    After - Before.

% remove trailing "\n"
chop(String) ->
	string:strip(String, right, 10).

init_seed()->
    now().

now_sec() ->
	{MSec, Seconds, MicroSec} = now(),
	Seconds+1000000*MSec.
	

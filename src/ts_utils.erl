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

-include("ts_profile.hrl").

%% user interface
-export([debug/3, debug/4, get_val/1, init_seed/0, chop/1, elapsed/2,
         now_sec/0, inet_setopts/4, node_to_hostname/1, add_time/2,
         level2int/1, mkey1search/2, close_socket/2, datestr/0, datestr/1]).

level2int("debug")     -> ?DEB;
level2int("info")      -> ?INFO;
level2int("notice")    -> ?NOTICE;
level2int("warning")   -> ?WARN;
level2int("error")     -> ?ERR;
level2int("critical")  -> ?CRIT;
level2int("emergency") -> ?EMERG.

%%----------------------------------------------------------------------
%% Func: get_val/1
%% Purpose: return environnement variable value for the current application
%% Returns: Value | undef_var
%%----------------------------------------------------------------------
get_val(Var) ->
	case application:get_env(Var) of 
		{ok, Val} ->
			Val;
		_ ->
			?LOGF("WARNING, env ~p is not defined ! ~n", [Var], ?ERR),
			undef_var
	end.

%%----------------------------------------------------------------------
%% Func: debug/3
%% Purpose: print debug message if level is high enough
%%----------------------------------------------------------------------
debug(From, Message, Level) ->
	debug(From, Message, [], Level).

debug(From, Message, Args, Level) ->
	Debug_level = ?config(debug_level),
	if 
		Level =< Debug_level ->
			error_logger:info_msg("~20s:(~p:~p) "++ Message,
					  [From, Level, self()] ++ Args);
		true ->
			nodebug
	end.

%%----------------------------------------------------------------------
%% Func: elapsed/2
%% Purpose: print elapsed time in microseconds
%% Returns: integer
%%----------------------------------------------------------------------
elapsed({Before1, Before2, Before3}, {After1, After2, After3}) ->
    After  = After1  * 1000000000  + After2  * 1000 + After3/1000,
    Before = Before1 * 1000000000  + Before2 * 1000 + Before3/1000,
    After - Before.

%%----------------------------------------------------------------------
%% Func: chop/1
%% Purpose: remove trailing "\n"
%%----------------------------------------------------------------------
chop(String) ->
	string:strip(String, right, 10).

%%----------------------------------------------------------------------
%% Func: init_seed/0
%%----------------------------------------------------------------------
init_seed()->
    now().

%%----------------------------------------------------------------------
%% Func: now_sec/0
%% Purpose: returns unix like elapsed time in sec
%%----------------------------------------------------------------------
now_sec() ->
	{MSec, Seconds, MicroSec} = now(),
	Seconds+1000000*MSec.

%%----------------------------------------------------------------------
%% Func: add_time/2
%% Purpose: add given Seconds to given Time (same format as now())
%%----------------------------------------------------------------------
add_time({MSec, Seconds, MicroSec}, SecToAdd) ->
    NewSec = Seconds +SecToAdd,
    case NewSec < 1000000 of
        true -> {MSec, NewSec, MicroSec};
        false ->{MSec+ (NewSec div 100000), NewSec-1000000, MicroSec}
    end.

%%----------------------------------------------------------------------
%% Func: inet_setopts/4
%% Purpose: set inet options depending on the protocol (gen_tcp, gen_udp,
%%  ssl)
%%----------------------------------------------------------------------
inet_setopts(Protocol, undefined, Opts, Pid) -> %socket was closed before
    ok;
inet_setopts(ssl, Socket, Opts, Pid) ->
	case ssl:setopts(Socket, Opts) of
		ok ->
			ok;
		{error, closed} ->
			ts_client:close(Pid);
		Error ->
			?LOGF("Error while setting ssl options ~p ~p ~n", [Opts, Error], ?ERR)
	end;
inet_setopts(gen_tcp, Socket,  Opts, Pid)->
	case inet:setopts(Socket, Opts) of
		ok ->
			ok;
		{error, closed} ->
			ts_client:close(Pid);
		Error ->
			?LOGF("Error while setting inet options ~p ~p ~n", [Opts, Error], ?ERR)
	end;
%% FIXME: UDP not tested
inet_setopts(gen_udp, Socket,  Opts, Pid)->
	ok = inet:setopts(Socket, Opts).

%5> inet:gethostbyname("schultze").
%{ok,{hostent,"schultze.ird.idealx.com",["schultze"],inet,4,[{192,168,0,181}]}}

node_to_hostname(Node) ->
    [Nodename, Hostname] = string:tokens( atom_to_list(Node), "@"),
    {ok, Hostname}.

%%----------------------------------------------------------------------
%% Func: mkey1search/2
%% Purpose: multiple key1search:
%% Take as input list of {Key, Value} tuples (length 2).
%% Return the list of values corresponding to a given key
%% It is assumed here that there might be several identical keys in the list
%% unlike the lists:key... functions.
%%----------------------------------------------------------------------
mkey1search(List, Key) ->
    Results = lists:foldl(
		fun({MatchKey, Value}, Acc) when MatchKey == Key ->
			[Value | Acc];
		   ({_OtherKey, _Value}, Acc) ->
			Acc 
		end,
		[],
		List),
    case Results of 
	[] -> undefined;
	Results -> lists:reverse(Results)
    end.

%% close socket if it exists
close_socket(Protocol, none) -> ok;
close_socket(gen_tcp, Socket)-> gen_tcp:close(Socket);
close_socket(ssl, Socket)    -> ssl:close(Socket);
close_socket(gen_udp, Socket)-> gen_udp:close(Socket).

%%----------------------------------------------------------------------
%% datestr/0
%% Purpose: print date as a string 'YYYY:MM:DD-HH:MM'
%%----------------------------------------------------------------------
datestr()->
    datestr(erlang:universaltime()).
%%----------------------------------------------------------------------
%% datestr/1
%%----------------------------------------------------------------------
datestr({{Y,M,D},{H,Min,S}})->
	io_lib:format("-~w:~w:~w-~w:~w",[Y,M,D,H,Min]).

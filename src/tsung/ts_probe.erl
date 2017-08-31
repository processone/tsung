%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2004 IDEALX
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

%%% In addition, as a special exception, you have the permission to
%%% link the code of this program with any library released under
%%% the EPL license and distribute linked combinations including
%%% the two.

%%% File    : ts_jabber.erl
%%% Author  : Nicolas Niclausse <nniclausse@IDEALX.com>
%%% Purpose :
%%% Created : 11 Jan 2004 by Nicolas Niclausse <nniclausse@IDEALX.com>

-module(ts_probe).
-author('nniclausse@hyperion').

-behavior(ts_plugin).

-include("ts_profile.hrl").
-include("ts_config.hrl").
-include("xmerl.hrl").

-export([add_dynparams/4,
         get_message/2,
         session_defaults/0,
         subst/2,
         parse/2,
         parse_bidi/2,
         dump/2,
         parse_config/2,
         decode_buffer/2,
         new_session/0,
         loop/1]).

-record(probe, {
          data,
          interval,
          socket
}).

session_defaults() ->
    {ok,true,true}.

%% @spec decode_buffer(Buffer::binary(),Session::record(raw)) ->  NewBuffer::binary()
%% @doc We need to decode buffer (remove chunks, decompress ...) for
%%      matching or dyn_variables
%% @end
decode_buffer(Buffer,#probe{}) ->
    Buffer.

new_session() ->
    #probe{}.

get_message(#probe{data=Data}, #state_rcv{session=S, socket=none })->
    %% first message, socket not yet opened, just build the message
    {Data,S};
get_message(#probe{data=Data, interval=Interval}, State =#state_rcv{session=S, socket=Socket }) when Interval > 0->
    Protocol =  State#state_rcv.protocol,
    Host = State#state_rcv.host,
    Port = State#state_rcv.port,
    Dump = State#state_rcv.dump,
    ?LOGF("Spawning background probe ~p",[Interval],?NOTICE),
    spawn(ts_probe, loop, [{Data, Interval, Socket, Host, Port, Protocol,Dump}]),
    {Data,S};
get_message(#probe{data=Data}, #state_rcv{session=S })->
    {Data,S}.


loop({Data, Interval, Socket, Host, Port, Protocol,Dump}) ->
    ?LOGF("Probe: sleeping ~p ms",[Interval],?DEB),
    timer:sleep(Interval),
    case Protocol:send(Socket,Data,[{host,Host},{port,Port}]) of
        ok ->
            ts_mon_cache:add([{ sum, size_sent, size(Data)},{count, probe}]),
            ts_mon:sendmes({Dump, self(), Data}),
            ?LOGF("Sending probe data to socket ~p ~p ~p ",[Socket, Host, Port],?DEB),
            loop({Data,Interval,Socket, Host, Port, Protocol,Dump});
        {error, Reason} ->
            ?LOGF("Error while sending probe data to socket ~p (~p:~p)",[Reason, Host, Port],?ERR)
    end.


parse(_Data, State) ->
    State.

parse_bidi(Data, State) ->
    {nodata, State, continue}.

dump(A,B) ->
    ts_plugin:dump(A,B).

%%
parse_config(Element = #xmlElement{name=dyn_variable}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
parse_config(Element = #xmlElement{name=probe, attributes=Attrs},
             Config=#config{curid = Id, session_tab = Tab,
                            sessions = [CurS | _], dynvar=DynVar,
                            subst    = SubstFlag, match=MatchRegExp}) ->
    Ack  = ts_config:getAttr(atom,Attrs, ack, no_ack),
    Data  = ts_config:getAttr(string, Attrs, data),
    Interval  = ts_config:getAttr(integer, Attrs, interval, 0),
    Req = #probe{data=list_to_binary(Data), interval=Interval},
    ts_config:mark_prev_req(Id-1, Tab, CurS),
    Msg=#ts_request{ack     = Ack,
                    subst   = SubstFlag,
                    match   = MatchRegExp,
                    param   = Req},
    ets:insert(Tab,{{CurS#session.id, Id},Msg#ts_request{endpage=true,
                                                         dynvar_specs=DynVar}}),
    lists:foldl( fun(A,B)->ts_config:parse(A,B) end,
                 Config#config{dynvar=[]},
                 Element#xmlElement.content);
%% Parsing other elements
parse_config(Element = #xmlElement{}, Conf = #config{}) ->
    ts_config:parse(Element,Conf);
%% Parsing non #xmlElement elements
parse_config(_, Conf = #config{}) ->
    Conf.

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: add dynamic parameters to build the message
%%----------------------------------------------------------------------
add_dynparams(_,[], Param, _Host) ->
    Param;
add_dynparams(true, {DynVars, _Session}, OldReq, _Host) ->
    subst(OldReq, DynVars);
add_dynparams(_Subst, _DynData, Param, _Host) ->
    Param.

%%----------------------------------------------------------------------
%% Function: subst/1
%%----------------------------------------------------------------------
subst(Req=#probe{data=Data},DynVars) ->
    Req#probe{ data= ts_search:subst(Data, DynVars)}.

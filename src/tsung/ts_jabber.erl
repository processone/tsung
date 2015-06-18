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
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Purpose : Jabber/XMPP plugin
%%% Created : 11 Jan 2004 by Nicolas Niclausse <nicolas@niclux.org>

-module(ts_jabber).
-author('nniclausse@hyperion').

-behavior(ts_plugin).

-include("ts_macros.hrl").
-include("ts_profile.hrl").
-include("ts_jabber.hrl").

-export([add_dynparams/4,
         get_message/2,
         session_defaults/0,
         subst/2,
         parse/2,
         dump/2,
         parse_bidi/2,
         parse_config/2,
         decode_buffer/2,
         new_session/0,
         username/2,
         userid/1]).

-export ([starttls_bidi/2,
          message_bidi/2,
          presence_bidi/2]).

%%----------------------------------------------------------------------
%% Function: session_default/0
%% Purpose: default parameters for session (persistent & bidirectional)
%% Returns: {ok, true|false, true|false}
%%----------------------------------------------------------------------
session_defaults() ->
    {ok, true, false}.

%% @spec decode_buffer(Buffer::binary(),Session::record(jabber)) ->  NewBuffer::binary()
%% @doc We need to decode buffer (remove chunks, decompress ...) for
%%      matching or dyn_variables
%% @end
decode_buffer(Buffer,#jabber_session{}) ->
    Buffer. % nothing to do for jabber


%% @spec userid({Session::record(jabber_session), Dynvars::dynvars()}) ->  UID::string()
%% @doc  return the current userid  @end
userid({#jabber_session{username=UID},_DynVars})-> UID.



%%----------------------------------------------------------------------
%% Function: new_session/0
%% Purpose: initialize session information
%% Returns: record or []
%%----------------------------------------------------------------------
new_session() ->
    #jabber_session{}.
%%----------------------------------------------------------------------
%% Function: get_message/1
%% Purpose: Build a message/request
%% Args:    #jabber
%% Returns: binary
%%----------------------------------------------------------------------
get_message(Req=#jabber{domain={domain,Domain}}, State=#state_rcv{session=S}) when S#jabber_session.domain == undefined  ->
    NewS = S#jabber_session{domain=Domain, user_server=default},
    get_message(Req#jabber{domain=Domain, user_server=default},State#state_rcv{session=NewS});

get_message(Req=#jabber{domain={vhost,FileId}}, State=#state_rcv{session=S}) when S#jabber_session.domain == undefined  ->
    {Domain,UserServer} = choose_domain(FileId),
    NewS = S#jabber_session{domain=Domain, user_server=UserServer},
    get_message(Req#jabber{domain=Domain, user_server=UserServer},State#state_rcv{session=NewS});

get_message(Req=#jabber{id=user_defined, username=User, passwd=Passwd}, State=#state_rcv{session=S}) when S#jabber_session.id == undefined  ->
    NewS = S#jabber_session{id=user_defined,username=User,passwd=Passwd},
    %% NewDynVars =ts_dynvars:set(xmpp_userid, User, DynData#dyndata.dynvars),
    %% ?LOGF("Setting up username ~p for ~p~n",[User,ts_dynvars:lookup(tsung_userid,NewDynVars)],?DEB),
    get_message(Req, State#state_rcv{session=NewS});

get_message(Req=#jabber{prefix=Prefix, passwd=Passwd}, State=#state_rcv{session=S}) when S#jabber_session.id == undefined  ->
   Id = case ts_user_server:get_idle(S#jabber_session.user_server) of
             {error, no_free_userid} ->
                 ts_mon:add({ count, error_no_free_userid }),
                 exit(no_free_userid);
             Val->
                Val
        end,
    {NewUser,NewPasswd} = {username(Prefix,Id), password(Passwd,Id)},
    %% NewDynVars =ts_dynvars:set(xmpp_userid, NewUser, DynData#dyndata.dynvars),
    %% ?LOGF("Setting up username ~p for ~p~n",[NewUser,ts_dynvars:lookup(tsung_userid,NewDynVars)],?DEB),
    NewS = S#jabber_session{id=Id,username=NewUser,passwd=NewPasswd},

    get_message(Req#jabber{username=NewUser,passwd=NewPasswd},State#state_rcv{session=NewS});

get_message(Req=#jabber{},#state_rcv{session=S}) ->
    {ts_jabber_common:get_message(Req),S}.


dump(A,B) ->
    ts_plugin:dump(A,B).

%%----------------------------------------------------------------------
%% Function: parse/2
%% Purpose: Parse the given data and return a new state
%% Args:    Data (binary)
%%            State (record)
%% Returns:  {NewState, Opts, Close}
%%  State = #state_rcv{}
%%  Opts = proplist()
%%  Close = bool()
%%----------------------------------------------------------------------
parse(closed, State) ->
    ?LOG("XMPP connection closed by server!",?WARN),
    {State#state_rcv{ack_done = true}, [], true};
parse(Data, State=#state_rcv{datasize=Size}) ->
    ?DebugF("RECEIVED : ~p~n",[Data]),
    case get(regexp) of
        undefined ->
            ?LOG("No regexp defined, skip",?WARN),
            {State#state_rcv{ack_done=true}, [], false};
        Regexp ->
            case re:run(Data, Regexp) of
                {match,_} ->
                    ?DebugF("XMPP parsing: Match (regexp was ~p)~n",[Regexp]),
                    {State#state_rcv{ack_done=true, datasize=Size+size(Data)}, [], false};
                nomatch ->
                    {State#state_rcv{ack_done=false,datasize=Size+size(Data)}, [], false}
            end
    end.

%%----------------------------------------------------------------------
%% Function: parse_bidi/2
%% Purpose: Parse the given data, return a response and new state
%% Args:    Data (binary)
%%          State (record)
%% Returns:    Data (binary)
%%             NewState (record)
%%----------------------------------------------------------------------
parse_bidi(Data,  State) ->
    RcvdXml = binary_to_list(Data),
    BidiElements =
        [{"<presence[^>]*subscribe[\"\']", presence_bidi},
         {"@@@([^@]+)@@@", message_bidi},
         {"<proceed", starttls_bidi}],
    lists:foldl(fun({Regex, Handler}, Acc)->
       case re:run(RcvdXml,Regex) of
        {match,_} ->
            ?LOGF("RECEIVED : ~p~n",[RcvdXml],?DEB),
            ?MODULE:Handler(RcvdXml, State);
        _Else ->
            Acc
        end
    end, {nodata, State, think}, BidiElements).

presence_bidi(RcvdXml, State)->
    {match,SubMatches} = re:run(RcvdXml,"<presence[^>]*subscribe[\"\'][^>]*>",[global]),
    bidi_resp(subscribed,RcvdXml,SubMatches,State).

message_bidi(RcvdXml, State) ->
    {match, [NodeStamp]} = re:run(RcvdXml, "@@@([^@]+)@@@", [{capture, all_but_first, list}]),
    [NodeS, StampS] = string:tokens(NodeStamp, ","),
    case integer_to_list(erlang:phash2(node())) of
        NodeS ->
            [MegaS, SecsS, MicroS] = string:tokens(StampS, ";"),
            Mega = list_to_integer(MegaS),
            Secs = list_to_integer(SecsS),
            Micro = list_to_integer(MicroS),
            Latency = timer:now_diff(?NOW, {Mega, Secs, Micro}),
            ts_mon:add({ sample, xmpp_msg_latency, Latency / 1000});
        _ ->
            ignore
    end,
    {nodata, State, think}.

starttls_bidi(_RcvdXml, #state_rcv{socket= Socket, send_timestamp=SendTime}=State)->
    ssl:start(),
    Req = subst(State#state_rcv.request#ts_request.param, State#state_rcv.dynvars),
    Opt = lists:filter(fun({_,V}) -> V /= undefined end, 
                      [{certfile,Req#jabber.certfile},
                       {keyfile,Req#jabber.keyfile},
                       {password,Req#jabber.keypass},
                       {cacertfile,Req#jabber.cacertfile}]),
    {ok, SSL} = ts_ssl:connect(Socket, Opt),
    ?LOGF("Upgrading to TLS : ~p",[SSL],?INFO),
    Latency = ts_utils:elapsed(SendTime, ?NOW),
    ts_mon:add({ sample, xmpp_starttls, Latency}),
    {nodata, State#state_rcv{socket=SSL,protocol=ts_ssl}, continue}.

%%----------------------------------------------------------------------
%% Function: bidi_resp/4
%% Purpose: Parse XMPP packet, build client response
%%          Accomodates single packets w/ multiple requests
%% Args:    RcvdXml (list)
%%          Submatches (list)
%%          State (record)
%% Returns:    Data (binary)
%%             NewState (record)
%%             think|continue
%%----------------------------------------------------------------------
%% subscribed: Complete a pending subscription request
bidi_resp(subscribed,RcvdXml,SubMatches,State) ->
    JoinedXml=lists:foldl(fun(X,Foo) ->
        [{Start,Len}]=X,
        SubStr = string:substr(RcvdXml,Start+1,Len),
        case re:run(SubStr,"from=[\"']([^\s]*)[\"'][\s\/\>]",[{capture,[1],list}]) of
            {match,[MyId]} ->
                %% MyId=string:substr(SubStr,Start1 +6, Length1 -8),
                ?LOGF("Subscription request from : ~p~n",[MyId],?DEB),
                MyXml = ["<presence to='", MyId, "' type='subscribed'/>"],
                lists:append([Foo],[MyXml]);
            _Else ->
                ?LOGF("Error getting sender address: ~p~n",[SubStr],?DEB),
                ""
        end
    end,"",SubMatches),
    case lists:flatten(JoinedXml) of
        "" ->
            {nodata,State, think};
        _ ->
            ?LOGF("RESPONSE TO SEND : ~s~n",[JoinedXml],?DEB),
            {list_to_binary(JoinedXml),State, think}
    end.

%%
parse_config(Element, Conf) ->
    ts_config_jabber:parse_config(Element, Conf).

%%----------------------------------------------------------------------
%% Function: add_dynparams/4
%% Purpose: add dynamic parameters to build the message
%%----------------------------------------------------------------------

%% The rest of the code expect to found a "domain" field in the #jabber request
%% with the domain of the jabber server (as string). We use the step of dynvars substitution
%% to choose and set the domain we want to connect, and keep that choice in the
%% process dictionary so we reuse it for all request made from the same session.
%% (see comments on choose_domain/1
%%
%% if we are testing a single domain (the default case), we  change from {domain,D}.
%% to the specified domain (D). If {vhost,FileId}, we choose a domain from that file
%% and set it.

%% first request in a session, do nothing
add_dynparams(Subst, {DynVars, S}, Param=#jabber{}, Host) when S#jabber_session.id == undefined ->
    add_dynparams2(Subst,DynVars, Param, Host);

add_dynparams(Subst, {DynVars, S}, Param=#jabber{}, Host) ->
    add_dynparams2(Subst,DynVars, Param#jabber{id=S#jabber_session.id,
                                               username=S#jabber_session.username,
                                               passwd=S#jabber_session.passwd,
                                               domain=S#jabber_session.domain,
                                               user_server=S#jabber_session.user_server},Host).


add_dynparams2(false,_, Param, _Host) ->
    Param;
add_dynparams2(true, DynVars, Param, _Host) ->
    ?DebugF("Subst in jabber msg (~p) with dyn vars ~p~n",[Param,DynVars]),
    NewParam = subst(Param, DynVars),
    updatejab(DynVars, NewParam).


%% This isn't ideal.. but currently there is no other way
%% than use side effects, as get_message/1 andn add_dynparams/4 aren't allowed
%% to return a new DynData, and so they can't modify the session state.
choose_domain(VHostFileId) ->
    {ok,DomainBin} = ts_file_server:get_random_line(VHostFileId),
    Domain=binary_to_list(DomainBin),
    UserServer = global:whereis_name(list_to_atom("us_"++Domain)),
    {Domain,UserServer}.

%%----------------------------------------------------------------------
%% Function: subst/2
%% Purpose: Replace on the fly dynamic element
%%----------------------------------------------------------------------
subst(Req=#jabber{id=user_defined, username=Name,passwd=Pwd, data=Data, resource=Resource}, Dynvars) ->
    NewUser = ts_search:subst(Name,Dynvars),
    NewPwd  = ts_search:subst(Pwd,Dynvars),
    NewData = ts_search:subst(Data,Dynvars),
    subst2(Req#jabber{username=NewUser,passwd=NewPwd,data=NewData,resource=ts_search:subst(Resource,Dynvars)}, Dynvars);

subst(Req=#jabber{data=Data,resource=Resource}, Dynvars) ->
    subst2(Req#jabber{data=ts_search:subst(Data,Dynvars),resource=ts_search:subst(Resource,Dynvars)},Dynvars).

subst2(Req=#jabber{type = Type}, Dynvars) when Type == 'starttls' ->
    Req#jabber{cacertfile = ts_search:subst(Req#jabber.cacertfile, Dynvars),
               keyfile = ts_search:subst(Req#jabber.keyfile, Dynvars),
               keypass = ts_search:subst(Req#jabber.keypass, Dynvars),
               certfile = ts_search:subst(Req#jabber.certfile, Dynvars)};
subst2(Req=#jabber{type = Type}, Dynvars) when Type == 'muc:chat' ; Type == 'muc:join'; Type == 'muc:nick' ; Type == 'muc:exit' ->
    Req#jabber{nick = ts_search:subst(Req#jabber.nick, Dynvars),
               room = ts_search:subst(Req#jabber.room, Dynvars)};
subst2(Req=#jabber{type = Type}, Dynvars) when Type == 'pubsub:create' ; Type == 'pubsub:subscribe'; Type == 'pubsub:publish'; Type == 'pubsub:delete' ->
    Req#jabber{node = ts_search:subst(Req#jabber.node, Dynvars)};
subst2(Req=#jabber{type = Type}, Dynvars) when Type == 'pubsub:unsubscribe' ->
    NewNode=ts_search:subst(Req#jabber.node,Dynvars),
    NewSubId=ts_search:subst(Req#jabber.subid,Dynvars),
    Req#jabber{node=NewNode,subid=NewSubId};
subst2(Req, _Dynvars) ->
    Req.

%%----------------------------------------------------------------------
%% Func: updatejab/2
%%  takes dyn vars and adds them to jabber record
%%  'nonce' used for sip-digest auth
%%  'sid' session-id used for digest auth
%%----------------------------------------------------------------------
updatejab(undefined,Param) -> Param;
updatejab([],Param) -> Param;
updatejab([{nonce, Val}|Rest], Param)->
   updatejab(Rest, Param#jabber{nonce = Val});
updatejab([{sid, Val}|Rest], Param)->
   updatejab(Rest, Param#jabber{sid = Val});
updatejab([_|Rest], Param)->
   updatejab(Rest, Param).



%%%----------------------------------------------------------------------
%%% Func: username/2
%%% Generate the username given a prefix and id
%%%----------------------------------------------------------------------
username(Prefix, DestId) when is_integer(DestId)->
    Prefix ++ integer_to_list(DestId);
username(Prefix, DestId) ->
    Prefix ++ DestId.

%%%----------------------------------------------------------------------
%%% Func: password/1
%%% Generate password for a given username
%%%----------------------------------------------------------------------
password(Prefix,Id) ->
    username(Prefix,Id).



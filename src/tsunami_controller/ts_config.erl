%%%  This code was developped by IDEALX (http://IDEALX.org/) and
%%%  contributors (their names can be found in the CONTRIBUTORS file).
%%%  Copyright (C) 2000-2003 IDEALX
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
%%%----------------------------------------------------------------------
%%% File    : config.erl
%%% Author  : Nicolas Niclausse <nicolas.niclausse@IDEALX.com>
%%% Purpose : Read the IDX-Tsunami XML config file. Currently, it
%%%           work by parsing the #xmlElement record by hand ! 
%%%           TODO: learn how to use xmerl correctly
%%% Created : 3 Dec 2003 by Nicolas Niclausse <nniclausse@idealx.com>
%%%----------------------------------------------------------------------

-module(ts_config).
-author('nniclausse@idealx.com').
-vc('$Id$ ').

-include("ts_profile.hrl").
-include("ts_config.hrl").

-include_lib("xmerl/inc/xmerl.hrl").

-export([read/1,
         getAttr/2,
         getAttr/3,
         parse/2,
         get_default/3
        ]).

%%%----------------------------------------------------------------------
%%% Function: read/1
%%% Purpose:  read the xml config file
%%%----------------------------------------------------------------------
read(Filename) ->
    case xmerl_scan:file(Filename) of
        {ok, Root = #xmlElement{}} ->  % xmerl-0.15
            ?LOGF("Reading config file: ~s~n", [Filename], ?NOTICE),
            Table = ets:new(sessiontable, [ordered_set, protected]),
            {ok, parse(Root, #config{session_tab = Table})};
        {Root = #xmlElement{}, Tail} ->  % xmerl-0.19
            ?LOGF("Reading config file: ~s~n", [Filename], ?NOTICE),
            Table = ets:new(sessiontable, [ordered_set, protected]),
            {ok, parse(Root, #config{session_tab = Table})};
        {error,Reason} ->
            {error, Reason}
    end.

%%%----------------------------------------------------------------------
%%% Function: parse/2
%%% Purpose:  parse the xmerl structure
%%%----------------------------------------------------------------------
parse(Element = #xmlElement{parents = []}, Conf=#config{}) ->
    Loglevel = getAttr(Element#xmlElement.attributes, loglevel),
    Dump     = getAttr(Element#xmlElement.attributes, dumptraffic),
    Monitor = case Dump of 
                  "false" -> none;
                  "true" -> full;
                  "light" -> light
              end,
    lists:foldl(fun parse/2,
		Conf#config{monitoring= Monitor, loglevel= ts_utils:level2int(Loglevel)},
		Element#xmlElement.content);


%% parsing the Server elements
parse(Element = #xmlElement{name=server}, Conf = #config{server=SList}) ->
    Server = getAttr(Element#xmlElement.attributes, host),
    Port   = getAttr(Element#xmlElement.attributes, port),
    Type = case getAttr(Element#xmlElement.attributes, type) of 
               "ssl" -> ssl;
               "tcp" -> gen_tcp;
               "udp" -> gen_udp
           end,

    {ok, [{integer,1,IPort}],1} = erl_scan:string(Port),

    lists:foldl(fun parse/2,
		Conf#config{server = #server{host=Server,
					       port=IPort,
					       type=Type
					      }},
		Element#xmlElement.content);

%% Parsing the cluster monitoring element (monitor)
parse(Element = #xmlElement{name=monitor}, Conf = #config{monitor_hosts=MHList}) ->
    Host = getAttr(Element#xmlElement.attributes, host),
    %% TODO:
    %% Type = getAttr(Element#xmlElement.attributes, type), % os_mon | snmp
    lists:foldl(fun parse/2,
		Conf#config{monitor_hosts = lists:append(MHList, [Host])},
		Element#xmlElement.content);


%% Parsing the Client element
parse(Element = #xmlElement{name=client},
      Conf = #config{clients=CList}) ->

    Host     = getAttr(Element#xmlElement.attributes, host),
    Weight   = getAttr(Element#xmlElement.attributes, weight),
    MaxUsers = getAttr(Element#xmlElement.attributes, maxusers),
    CPU = getAttr(Element#xmlElement.attributes, cpu, "1"),
    {ok, [{integer,1,ICPU}],1} = erl_scan:string(CPU),
    {ok, [{integer,1,IWeight}],1} = erl_scan:string(Weight),
    {ok, [{integer,1,IMaxUsers}],1} = erl_scan:string(MaxUsers),
    %% add a new client for each CPU
    NewClients=lists:duplicate(ICPU,#client{host     = Host,
                                           weight   = IWeight/ICPU,
                                           maxusers = IMaxUsers}),
    lists:foldl(fun parse/2, Conf#config{clients = lists:append(NewClients,CList)},
                Element#xmlElement.content);

%% Parsing the ip element
parse(Element = #xmlElement{name=ip},
      Conf = #config{clients=[CurClient|CList]}) ->
    IPList = CurClient#client.ip,

    StrIP     = getAttr(Element#xmlElement.attributes, value),
    {ok, IP } = inet:getaddr(StrIP,inet),

    lists:foldl(fun parse/2,
		Conf#config{clients = [CurClient#client{ip = [IP|IPList]}
                               |CList]},
                Element#xmlElement.content);

%% Parsing the arrivalphase element
parse(Element = #xmlElement{name=arrivalphase},
      Conf = #config{arrivalphases=AList}) ->

    PhaseStr     = getAttr(Element#xmlElement.attributes, phase),
    {ok, [{integer,1,Phase}],1} = erl_scan:string(PhaseStr),
    Duration  = getAttr(Element#xmlElement.attributes, duration),
    Unit  = getAttr(Element#xmlElement.attributes, unit, "second"),
    {ok, [{integer,1,IDuration}],1} = erl_scan:string(Duration),
	D = to_seconds(Unit, IDuration),
    lists:foldl(fun parse/2,
		Conf#config{arrivalphases = [#arrivalphase{phase=Phase,
                                                   duration=D
                                                  }
									 |AList]},
                Element#xmlElement.content);

%% Parsing the users element
parse(Element = #xmlElement{name=users},
      Conf = #config{arrivalphases=[CurA | AList]}) ->
    
    MaxNumber = case getAttr(Element#xmlElement.attributes, maxnumber, infinity) of
				 infinity -> 
					 infinity;
				 Val ->
					 {ok, [{integer,1,Max}],1} = erl_scan:string(Val),
					 Max
			 end,
	?LOGF("Maximum number of users ~p~n",[MaxNumber],?INFO),

    InterArrival  =    
        case erl_scan:string(getAttr(Element#xmlElement.attributes, interarrival)) of
            {ok, [{integer,1,I}],1} -> I;
            {ok, [{float,1,F}],1} -> F
        end,
    Unit  = getAttr(Element#xmlElement.attributes, unit, "second"),
    Intensity= 1/(1000 * to_seconds(Unit,InterArrival)),

    lists:foldl(fun parse/2,
		Conf#config{arrivalphases = [CurA#arrivalphase{maxnumber = MaxNumber,
                                                        intensity=Intensity}
                               |AList]},
                Element#xmlElement.content);

%% Parsing the session element
parse(Element = #xmlElement{name=session},
      Conf = #config{session_tab = Tab, curid= PrevReqId, sessions=SList}) ->

    Id = length(SList),
    Msg_ack     = getAttr(Element#xmlElement.attributes, messages_ack),
    Persistent  = getAttr(Element#xmlElement.attributes, persistent),
    Name        = getAttr(Element#xmlElement.attributes, name),
    Type        = getAttr(Element#xmlElement.attributes, type),
    ?LOGF("Session name for id ~p is ~p~n",[Id+1, Name],?NOTICE),
    Popularity = 
        case erl_scan:string(getAttr(Element#xmlElement.attributes, popularity)) of
            {ok, [{integer,1,IPop}],1} -> IPop;
            {ok, [{float,1,FPop}],1} -> FPop
        end,
        
    {ok, [{atom,1,AMsg_Ack}],1}    = erl_scan:string(Msg_ack),
    {ok, [{atom,1,APersistent}],1} = erl_scan:string(Persistent),
    {ok, [{atom,1,AType}],1}       = erl_scan:string(Type),

    case Id of 
        0 -> ok; % first session 
        _ -> ets:insert(Tab, {{Id, size}, PrevReqId}) 
             %% add total requests count in previous session in ets table
    end,
            
    lists:foldl(fun parse/2,
                Conf#config{sessions = [#session{id           = Id + 1,
                                                 popularity   = Popularity,
                                                 type         = AType,
                                                 messages_ack = AMsg_Ack,
                                                 persistent   = APersistent
                                                }
                                        |SList],
                            curid=0, cur_req_id=0},% re-initialize request id
                Element#xmlElement.content);

%%%% Parsing the transaction element
parse(Element = #xmlElement{name=transaction},
      Conf = #config{session_tab = Tab, sessions=[CurS|SList], curid=Id}) ->

    RawName = getAttr(Element#xmlElement.attributes, name),
    {ok, [{atom,1,Name}],1} = erl_scan:string("tr_"++RawName),
    ?LOGF("Add start transaction ~p in session ~p as id ~p",
         [Name,CurS#session.id,Id+1],?INFO),
    ets:insert(Tab, {{CurS#session.id, Id+1}, {transaction,start,Name}}),

    NewConf=lists:foldl( fun parse/2,
                 Conf#config{curid=Id+1},
                 Element#xmlElement.content),
    NewId = NewConf#config.curid,
    ?LOGF("Add end transaction ~p in session ~p as id ~p",
         [Name,CurS#session.id,NewId+1],?INFO),
    ets:insert(Tab, {{CurS#session.id, NewId+1}, {transaction,stop,Name}}),
    NewConf#config{curid=NewId+1} ;

%%% Parsing the request element
parse(Element = #xmlElement{name=request},
      Conf = #config{sessions=[CurSess|SList], curid=Id,cur_req_id=ReqId}) ->

    Type  = CurSess#session.type,

    lists:foldl( fun(A,B) ->Type:parse_config(A,B) end,
                 Conf#config{curid=Id+1, cur_req_id=Id+1},
                 Element#xmlElement.content);

%%% Parsing the default element
parse(Element = #xmlElement{name=default},
      Conf = #config{session_tab = Tab}) ->
    case getAttr(Element#xmlElement.attributes, type) of
        "" ->
            case getAttr(Element#xmlElement.attributes, name) of
                "thinktime" ->
                    Val = getAttr(Element#xmlElement.attributes, value),
                    {ok, [{integer,1,IThink}],1} = erl_scan:string(Val),
                    ets:insert(Tab,{{thinktime, value}, IThink}),
                    Random = getAttr(Element#xmlElement.attributes, random),
                    ets:insert(Tab,{{thinktime, random}, Random})
            end,
            lists:foldl( fun parse/2, Conf, Element#xmlElement.content);
        Type ->
            {ok, [{atom,1,Module}],1} = erl_scan:string(Type),
            Module:parse_config(Element, Conf)
    end;
            
            

%%% Parsing the thinktime element
parse(Element = #xmlElement{name=thinktime},
      Conf = #config{cur_req_id=ReqId, curid=Id, session_tab = Tab, 
                     sessions = [CurS |SList]}) ->
    case ets:lookup(Tab,{thinktime, value}) of 
        [] -> % no default value
            StrThink = getAttr(Element#xmlElement.attributes, value,"0"),
			{ok, [{integer,1,Think}],1} = erl_scan:string(StrThink),
            Randomize = case getAttr(Element#xmlElement.attributes, random) of
                            "true" -> true;
                            _      -> false
                        end;
        [{_Key, Think}] ->
            Randomize = case ets:lookup(Tab,{thinktime, random}) of 
                            [{_K, "true"}] -> true;
                            _              -> false
                        end
    end,
    RealThink = case Randomize of
                    true ->
						{random, Think * 1000};
%                        round(ts_stats:exponential(1/(Think*1000)));
                    false ->
                        round(Think * 1000)
                end,
    ?LOGF("New thinktime ~p for id (~p:~p)~n",[RealThink, CurS#session.id, Id+1],
          ?INFO),
    ets:insert(Tab,{{CurS#session.id, Id+1}, {thinktime, RealThink}}),
    [{Key, Msg}] = ets:lookup(Tab,{CurS#session.id, ReqId}),
    ets:insert(Tab,{Key, Msg#message{thinktime=RealThink, endpage=true}}),
    
    lists:foldl( fun parse/2, Conf#config{curthink=Think,curid=Id+1}, 
                 Element#xmlElement.content);


%% Parsing other elements
parse(Element = #xmlElement{}, Conf = #config{}) ->
    lists:foldl(fun parse/2, Conf, Element#xmlElement.content);

%% Parsing non #xmlElement elements
parse(Element, Conf = #config{}) ->
    Conf.


%%%----------------------------------------------------------------------
%%% Function: getAttr/2
%%% Purpose:  search the attibute list for the given one
%%%----------------------------------------------------------------------
getAttr(Attr, Name) -> getAttr(Attr, Name, "").

getAttr([Attr = #xmlAttribute{name=Name}|Tail], Name, Default) ->
    case Attr#xmlAttribute.value of
	[] -> Default;
	A  -> A
    end;

getAttr([H|T], Name, Default) ->
    getAttr(T, Name, Default);

getAttr([], Name, Default) ->
    Default.


%%%----------------------------------------------------------------------
%%% Function: getText/1
%%% Purpose:  get the text of the XML node
%%%----------------------------------------------------------------------
getText([Text = #xmlText{value=Value}|Tail]) -> build_list(
						  string:strip(Value, both));
getText(_Other)                              -> "".

%%%----------------------------------------------------------------------
%%% Function: to_seconds/2
%%% Purpose: get the real duration in seconds
%%%----------------------------------------------------------------------
to_seconds("second", Val)-> Val;
to_seconds("minute", Val)-> Val*60;
to_seconds("hour",   Val)-> Val*3600;
to_seconds("millisecond", Val)-> Val/1000.

%% Default separator is '%'
build_list(String) -> build_list(String, "%").
build_list(String, Sep) ->
    string:tokens(String, Sep).

%%
get_default(Tab, Key,ConfigName) ->
    case ets:lookup(Tab,{Key, value}) of 
		[] ->
			?config(ConfigName);
		[{_Key, SName}] ->
			SName
	end.

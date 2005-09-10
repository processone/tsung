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
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two.

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

-include("xmerl.hrl").

-export([read/1,
         getAttr/2,
         getAttr/3,
         getAttr/4,
         getText/1,
         parse/2,
         get_default/3,
		 mark_prev_req/3,
         get_batch_nodes/1
        ]).

%%%----------------------------------------------------------------------
%%% Function: read/1
%%% Purpose:  read the xml config file
%%%----------------------------------------------------------------------
read(Filename) ->
    case catch xmerl_scan:file(Filename,
                               [{fetch_path,["/usr/share/idx-tsunami/","./"]},
                                {validation,true}]) of
                                                % FIXME:validation doesn't work ?
        {ok, Root = #xmlElement{}} ->  % xmerl-0.15
            ?LOGF("Reading config file: ~s~n", [Filename], ?NOTICE),
            Table = ets:new(sessiontable, [ordered_set, protected]),
            {ok, parse(Root, #config{session_tab = Table})};
        {Root = #xmlElement{}, _Tail} ->  % xmerl-0.19
            ?LOGF("Reading config file: ~s~n", [Filename], ?NOTICE),
            Table = ets:new(sessiontable, [ordered_set, protected]),
            {ok, parse(Root, #config{session_tab = Table})};
        {error,Reason} ->
            {error, Reason};
		{'EXIT',Reason} ->
            {error, Reason}
    end.

%%%----------------------------------------------------------------------
%%% Function: parse/2
%%% Purpose:  parse the xmerl structure
%%%----------------------------------------------------------------------
parse(Element = #xmlElement{parents = [], attributes=Attrs}, Conf=#config{}) ->
    Loglevel = getAttr(Attrs, loglevel),
    Dump     = getAttr(Attrs, dumptraffic),
    BackEnd  = getAttr(atom, Attrs, backend, text),
    DumpType = case Dump of 
                   "false" -> none;
                   "true"  -> full;
                   "light" -> light
               end,
    lists:foldl(fun parse/2,
                Conf#config{dump= DumpType, stats_backend=BackEnd,
                            loglevel= ts_utils:level2int(Loglevel)},
                Element#xmlElement.content);


%% parsing the Server elements
parse(Element = #xmlElement{name=server, attributes=Attrs}, Conf) ->
    Server = getAttr(Attrs, host),
    Port   = getAttr(integer, Attrs, port),
    Type = case getAttr(Attrs, type) of 
               "ssl" -> ssl;
               "tcp" -> gen_tcp;
               "udp" -> gen_udp
           end,

    lists:foldl(fun parse/2,
		Conf#config{server = #server{host=Server,
					       port=Port,
					       type=Type
					      }},
		Element#xmlElement.content);

%% Parsing the cluster monitoring element (monitor)
parse(Element = #xmlElement{name=monitor, attributes=Attrs},
      Conf = #config{monitor_hosts=MHList}) ->
    Host = getAttr(Attrs, host),
    Type = getAttr(atom, Attrs, type, erlang),
    NewMon = case getAttr(atom, Attrs, batch, false) of 
                 true ->
                     Nodes = get_batch_nodes(Host),
                     lists:map(fun(N)-> {N, Type} end, Nodes);
                 _ ->
                     [{Host, Type}]
             end,
    lists:foldl(fun parse/2,
		Conf#config{monitor_hosts = lists:append(MHList, NewMon)},
		Element#xmlElement.content);


%% Parsing the Client element
parse(Element = #xmlElement{name=client, attributes=Attrs},
      Conf = #config{clients=CList}) ->
    Host     = getAttr(Attrs, host),
    Weight   = getAttr(integer,Attrs, weight,1),
    MaxUsers = getAttr(integer,Attrs, maxusers,750),
    SingleNode = getAttr(atom, Attrs, use_controller_vm, false) or Conf#config.use_controller_vm,
    NewClients =
        case getAttr(atom, Attrs, type) of
            batch ->
                Batch = getAttr(atom, Attrs, batch),
                Nodes = get_batch_nodes(Batch),
                Fun = fun(N)-> #client{host=N,weight=Weight,maxusers=MaxUsers} end,
                lists:map(Fun, Nodes);
            _ ->
                CPU      = getAttr(integer,Attrs, cpu, 1),
                %% must be hostname and not ip:
                case ts_utils:is_ip(Host) of
                    true ->
                        ?LOGF("ERROR: client config: 'host' attribute must be a hostname, "++
                              "not an IP ! (was ~p)~n",[Host],?EMERG),
                        throw({error, badhostname});
                    false ->
                        %% add a new client for each CPU
                        lists:duplicate(CPU,#client{host     = Host,
                                                    weight   = Weight/CPU,
                                                    maxusers = MaxUsers})
                end
        end,
    lists:foldl(fun parse/2,
                Conf#config{clients = lists:append(NewClients,CList),
                            use_controller_vm = SingleNode},
                Element#xmlElement.content);

%% Parsing the ip element
parse(Element = #xmlElement{name=ip, attributes=Attrs},
      Conf = #config{clients=[CurClient|CList]}) ->
    IPList = CurClient#client.ip,

    StrIP     = getAttr(Attrs, value),
    {ok, IP } = inet:getaddr(StrIP,inet),

    lists:foldl(fun parse/2,
		Conf#config{clients = [CurClient#client{ip = [IP|IPList]}
                               |CList]},
                Element#xmlElement.content);

%% Parsing the arrivalphase element
parse(Element = #xmlElement{name=arrivalphase, attributes=Attrs},
      Conf = #config{arrivalphases=AList}) ->

    Phase     = getAttr(integer,Attrs, phase),
    IDuration  = getAttr(integer, Attrs, duration),
    Unit  = getAttr(string,Attrs, unit, "second"),
	D = to_seconds(Unit, IDuration),
    case lists:keysearch(Phase,#arrivalphase.phase,AList) of
        false ->
            lists:foldl(fun parse/2,
                        Conf#config{arrivalphases = [#arrivalphase{phase=Phase,
                                                                   duration=D
                                                                  }
                                                     |AList]},
                        Element#xmlElement.content);
        _ -> % already existing phase, wrong configuration.
            ?LOGF("Client config error: phase ~p already defined, abort !~n",[Phase],?EMERG),
            throw({error, already_defined_phase})
    end;
        
%% Parsing the users element
parse(Element = #xmlElement{name=users, attributes=Attrs},
      Conf = #config{arrivalphases=[CurA | AList]}) ->
    
    Max = getAttr(integer,Attrs, maxnumber, infinity),
	?LOGF("Maximum number of users ~p~n",[Max],?INFO),

    InterArrival  = getAttr(float_or_integer,Attrs, interarrival),
    Unit  = getAttr(string,Attrs, unit, "second"),
    Intensity= 1/(1000 * to_seconds(Unit,InterArrival)),

    lists:foldl(fun parse/2,
		Conf#config{arrivalphases = [CurA#arrivalphase{maxnumber = Max,
                                                        intensity=Intensity}
                               |AList]},
                Element#xmlElement.content);

%% Parsing the session element
parse(Element = #xmlElement{name=session, attributes=Attrs},
      Conf = #config{session_tab = Tab, curid= PrevReqId, sessions=SList}) ->

    Id = length(SList),
    Type        = getAttr(atom,Attrs, type),

    {ok, Ack_def, Persistent_def} = Type:session_defaults(),

    Msg_Ack     = getAttr(atom,Attrs, messages_ack, Ack_def),
    Persistent  = getAttr(atom,Attrs, persistent, Persistent_def),

    Name        = getAttr(Attrs, name),
    ?LOGF("Session name for id ~p is ~p~n",[Id+1, Name],?NOTICE),
    ?LOGF("Session type: ack=~p persistent=~p~n",[Msg_Ack, Persistent],?NOTICE),
    Popularity = getAttr(float_or_integer, Attrs, popularity),
    case Id of 
        0 -> ok; % first session 
        _ -> 
            %% add total requests count in previous session in ets table
            ets:insert(Tab, {{Id, size}, PrevReqId}) 
    end,
            
    lists:foldl(fun parse/2,
                Conf#config{sessions = [#session{id           = Id + 1,
                                                 popularity   = Popularity,
                                                 type         = Type,
                                                 messages_ack = Msg_Ack,
                                                 persistent   = Persistent,
                                                 ssl_ciphers  = Conf#config.ssl_ciphers
                                                }
                                        |SList],
                            curid=0, cur_req_id=0},% re-initialize request id
                Element#xmlElement.content);

%%%% Parsing the transaction element
parse(Element = #xmlElement{name=transaction, attributes=Attrs},
      Conf = #config{session_tab = Tab, sessions=[CurS|_], curid=Id}) ->

    RawName = getAttr(Attrs, name),
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

%%% Parsing the dyn_variable element
parse(Element = #xmlElement{name=dyn_variable, attributes=Attrs},
      Conf=#config{sessions=[CurS|_],dynvar=DynVar}) ->
    StrName  = getAttr(Attrs, name),
    DefaultRegExp = "name=(\"|')"++ StrName ++"(\"|') +value=(\"|')\\([^\"]+\\)(\"|')",%'
    RegExp  = getAttr(string,Attrs, regexp, DefaultRegExp),
    {ok, [{atom,1,Name}],1} = erl_scan:string(StrName),
    ?LOGF("Add new regexp: ~s ~n", [RegExp],?INFO),
    %% precompilation of the regexp
    {ok, RegExpStr} = gregexp:parse(lists:flatten(RegExp)),
    NewDynVar = case DynVar of 
                    undefined ->[{Name, RegExpStr}];
                    _->[{Name, RegExpStr}|DynVar]
                end,
    ?LOGF("Add new dyn variable=~p in session ~p~n",
         [NewDynVar,CurS#session.id],?INFO),
    Conf#config{ dynvar= NewDynVar };

%%% Parsing the request element
parse(Element = #xmlElement{name=request, attributes=Attrs},
      Conf = #config{sessions=[CurSess|_], curid=Id}) ->

    Type  = CurSess#session.type,
    SubstitutionFlag  = getAttr(atom,Attrs, subst, false),
    MatchRegExp  = getAttr(string,Attrs, match, undefined),

    %% we must parse dyn_variable before; unfortunately, there is no
    %% lists:keysort with Fun. FIXME: this will not work if a protocol
    %% name is sorted before 'dyn_variable'
    SortedContent = lists:keysort(#xmlElement.name, Element#xmlElement.content),

    lists:foldl( fun(A,B) ->Type:parse_config(A,B) end,
                 Conf#config{curid=Id+1, cur_req_id=Id+1,
                             subst=SubstitutionFlag,
                             match=MatchRegExp
                            },
                 SortedContent);

%%% Parsing the default element
parse(Element = #xmlElement{name=default, attributes=Attrs},
      Conf = #config{session_tab = Tab}) ->
    case getAttr(atom,Attrs, type) of
        "" ->
            case getAttr(Attrs, name) of
                "thinktime" ->
                    Val = getAttr(integer,Attrs, value),
                    ets:insert(Tab,{{thinktime, value}, Val}),
                    Random = getAttr(string,Attrs, random,
                                     ?config(thinktime_random)),
                    ets:insert(Tab,{{thinktime, random}, Random}),
                    Override = getAttr(string, Attrs, override,
                                       ?config(thinktime_override)),
                    ets:insert(Tab,{{thinktime, override}, Override}),
                    lists:foldl( fun parse/2, Conf, Element#xmlElement.content);
                "ssl_ciphers" ->
                    Cipher = getAttr(string,Attrs, value, negociate),
                    lists:foldl( fun parse/2, Conf#config{ssl_ciphers=Cipher},
                                 Element#xmlElement.content);
                "file_server" ->
                    FileName = getAttr(Attrs, value),
                    lists:foldl( fun parse/2, Conf#config{file_server=FileName},
                                 Element#xmlElement.content);
                _ ->                    
                    lists:foldl( fun parse/2, Conf, Element#xmlElement.content)
            end;
        Module ->
            Module:parse_config(Element, Conf)
    end;
            
            

%%% Parsing the thinktime element
parse(Element = #xmlElement{name=thinktime, attributes=Attrs},
      Conf = #config{cur_req_id=ReqId, curid=Id, session_tab = Tab, 
                     sessions = [CurS |_]}) ->
    DefThink = get_default(Tab,{thinktime, value},thinktime_value),
    DefRandom = get_default(Tab,{thinktime, random},thinktime_random),
    {Think, Randomize} = 
        case get_default(Tab,{thinktime, override},thinktime_override) of
            "true" -> 
                {DefThink, DefRandom};
            "false" ->
                CurThink = getAttr(integer, Attrs, value,DefThink),
                CurRandom=getAttr(string, Attrs,random,DefRandom),
                {CurThink, CurRandom}
        end,
    RealThink = case Randomize of
                    "true" ->
						{random, Think * 1000};
                    "false" ->
                        round(Think * 1000)
                end,
    ?LOGF("New thinktime ~p for id (~p:~p)~n",[RealThink, CurS#session.id, Id+1],
          ?INFO),
    ets:insert(Tab,{{CurS#session.id, Id+1}, {thinktime, RealThink}}),
    [{Key, Msg}] = ets:lookup(Tab,{CurS#session.id, ReqId}),
    ets:insert(Tab,{Key, Msg#ts_request{thinktime=RealThink}}),
    
    lists:foldl( fun parse/2, Conf#config{curthink=Think,curid=Id+1}, 
                 Element#xmlElement.content);

%% Parsing other elements
parse(Element = #xmlElement{}, Conf = #config{}) ->
    lists:foldl(fun parse/2, Conf, Element#xmlElement.content);

%% Parsing non #xmlElement elements
parse(_Element, Conf = #config{}) ->
    Conf.


%%%----------------------------------------------------------------------
%%% Function: getAttr/2
%%% Purpose:  search the attibute list for the given one
%%%----------------------------------------------------------------------
getAttr(Attr, Name) -> getAttr(string, Attr, Name, "").
getAttr(Type, Attr, Name) -> getAttr(Type, Attr, Name, "").

getAttr(Type, [Attr = #xmlAttribute{name=Name}|_], Name, Default) ->
    case Attr#xmlAttribute.value of
        [] -> Default;
        A  -> getTypeAttr(Type,A)
    end;

getAttr(Type, [_H|T], Name, Default) ->
    getAttr(Type, T, Name, Default);

getAttr(_Type, [], _Name, Default) ->
    Default.

getTypeAttr(string, String)-> String;
getTypeAttr(list, String)-> String;
getTypeAttr(float_or_integer, String)->
    case erl_scan:string(String) of
        {ok, [{integer,1,I}],1} -> I;
        {ok, [{float,1,F}],1} -> F
    end;
getTypeAttr(Type, String) ->
    {ok, [{Type,1,Val}],1} = erl_scan:string(String),
    Val.


%%%----------------------------------------------------------------------
%%% Function: getText/1
%%% Purpose:  get the text of the XML node
%%%----------------------------------------------------------------------
getText([#xmlText{value=Value}|_]) -> build_list(string:strip(Value, both));
getText(_Other) -> "".

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

%%%----------------------------------------------------------------------
%%% Function: get_default/2
%%%----------------------------------------------------------------------
get_default(Tab, Key,ConfigName) when not is_tuple(Key) ->
    get_default(Tab, {Key, value},ConfigName);
get_default(Tab, Key,ConfigName) ->
    case ets:lookup(Tab,Key) of 
		[] ->
			?config(ConfigName);
		[{_, SName}] ->
			SName
	end.

%%%----------------------------------------------------------------------
%%% Function: mark_prev_req/3
%%% Purpose: use to set page marks in requests during parsing ; by
%%%   default, a new request is mark as an endpage; if a new request is
%%%   parse, then the previous one must be set to false, unless there is
%%%   a thinktime between them
%%%----------------------------------------------------------------------
mark_prev_req(0, _, _)  ->
	ok;
mark_prev_req(Id, Tab, CurS) ->
    %% if the previous msg is a #ts_request request, set endpage to
    %% false, we are the current last request of the page
	case ets:lookup(Tab,{CurS#session.id, Id}) of 
		[{Key, Msg=#ts_request{}}] ->
			ets:insert(Tab,{Key, Msg#ts_request{endpage=false}});
		[{_, {transaction,_,_}}] ->% transaction, continue to search back
			mark_prev_req(Id-1, Tab, CurS);
		_ -> ok
	end.


get_batch_nodes(pbs) ->
    get_batch_nodes(torque);
get_batch_nodes(lsf)->
    case os:getenv("LSB_HOSTS") of
        false ->
            [];
        Nodes -> 
            lists:map(fun shortnames/1, string:tokens(Nodes, " "))

    end;
get_batch_nodes(oar) -> get_batch_nodes2("OAR_NODEFILE");
get_batch_nodes(torque) -> get_batch_nodes2("PBS_NODEFILE").

get_batch_nodes2(Env) ->
    case os:getenv(Env) of
        false ->
            [];
        NodeFile ->
            {ok, Nodes} = ts_utils:file_to_list(NodeFile),
            lists:map(fun shortnames/1, Nodes)
    end.

shortnames(Hostname)->
    [S | _]= string:tokens(Hostname,"."),
    S.

%% ts_user_server_sup.erl
%% @author Pablo Polvorin
%% @doc
%% created on 2008-09-09
-module(ts_user_server_sup).

-export([start_link/0,init/1,start_user_server/1,all_children/0]).

-behaviour(supervisor).

start_link() ->
    {ok,Pid} = supervisor:start_link({global,?MODULE},?MODULE,[]),
    start_default_user_server(),
    %default user_server is always started
    {ok,Pid}.

init([]) ->
    SupFlags = {simple_one_for_one,1,1 },
    ChildSpec = [
                 {ts_user_server,{ts_user_server, start, []},
                  temporary,2000,worker,[ts_user_server]}
                ],
    {ok, {SupFlags, ChildSpec}}.

start_user_server(Name) ->
   supervisor:start_child({global,?MODULE},[Name]).

start_default_user_server() ->
   supervisor:start_child({global,?MODULE},[]).


all_children() ->
    [ Pid ||{_,Pid,_,_} <- supervisor:which_children({global,?MODULE})].

%%%  Author : Sebastian Cohnen <mail@tisba.de>
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%
%%%  In addition, as a special exception, you have the permission to
%%%  link the code of this program with any library released under
%%%  the EPL license and distribute linked combinations including
%%%  the two; the MPL (Mozilla Public License), which EPL (Erlang
%%%  Public License) is based on, is included in this exception.

-module(ts_module_distribution).

-vc('$Id$ ').
-author('Sebastian Cohnen <mail@tisba.de>').

-include("ts_config.hrl").

-export([ distribute_modules/2, load_modules_locally/1 ]).

-define(RPC_TIMEOUT, 30000).

load_modules_locally(#config{modules_source=Paths, modules_beam=Modules}=Config) ->
    lists:foreach(
        fun({Module, Binary, Filename}) ->
            code:load_binary(Module, Filename, Binary)
        end, compile_modules(Paths) ++ load_modules(Modules)).

distribute_modules(Nodes, #config{modules_source=Paths, modules_beam=Modules}) ->
    ModuleSpecs = compile_modules(Paths) ++ load_modules(Modules),
    distribute_modules_to_nodes(Nodes, ModuleSpecs).

compile_modules([]) -> [];
compile_modules(Paths) ->
    lists:map(fun(Path) ->
        case compile:file(Path, [binary, compressed, return]) of
            {error, Reason} ->
                ?LOGF("Module from code ~p could not be compiled! Reason: ~p~n", [Path, Reason], ?ERR),
                exit(module_compilation_error);
            {ok, Module, Binary, []} ->
                ?LOGF("Module ~p from ~p was compiled successfully!~n", [Module, Path], ?NOTICE),
                {Module, Binary, Path};
            {ok, Module, Binary, Warnings} ->
                ?LOGF("There were warnings when compiling module ~p from ~p: ~p~n", [Module, Path, Warnings], ?WARN),
                {Module, Binary, Path}
        end
    end, Paths).

load_modules([]) -> [];
load_modules(Modules) ->
    lists:map(fun(Module) ->
        case code:get_object_code(Module) of
            error ->
                ?LOGF("Module ~p could not be loaded! Check given load paths.~n", [Module], ?ERR),
                exit(module_load_error);
            {Module, Binary, Filename} ->
                ?LOGF("Module ~p from ~p was loaded successfully!~n", [Module, Filename], ?NOTICE),
                {Module, Binary, Filename}
        end
    end, Modules).

distribute_modules_to_nodes([], _) -> ok;
distribute_modules_to_nodes(_, []) -> ok;
distribute_modules_to_nodes(Nodes, Modules) ->
    Fun = fun(Module) -> distribute_module(Nodes, Module) end,
    DistributionResult = ts_utils:pmap(Fun, Modules, 10),

    case lists:foreach(fun(Status) -> Status == ok end, DistributionResult) of
        true ->
            ok;
        false ->
            ts_mon:abort(),
            exit(module_distribution_error)
    end.

% Distribute given {Module, Binary, Filename} to all given Nodes
distribute_module(Nodes, {Module, Binary, Filename}) ->
    {_Resl, BadNodes} = rpc:multicall(Nodes, code, load_binary, [Module, Filename, Binary], ?RPC_TIMEOUT),
    case BadNodes of
        [] ->
            ok;
        Bad ->
            ?LOGF("Can't distribute ~n to all nodes ~p~n", [Module, Bad], ?ERR),
            error
    end.

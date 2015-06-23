%%%-------------------------------------------------------------------
%%% File    : ts_test_all.erl
%%% Author  : Nicolas Niclausse <nicolas@niclux.org>
%%% Description : run all test functions
%%%
%%% Created : 17 Mar 2007 by Nicolas Niclausse <nicolas@niclux.org>
%%%-------------------------------------------------------------------
-module(ts_test_all).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-define(FILE_NAME(MODULE), "cover_report/" ++ atom_to_list(MODULE) ++ ".html").


version() ->
    case re:run(erlang:system_info(otp_release), "R?(\\d+)B?-?(\\d+)?", [{capture, all, list}]) of
        {match, [_Full, Maj, Min]} ->
            {list_to_integer(Maj), list_to_integer(Min)};
        {match, [_Full, Maj]} ->
            {list_to_integer(Maj), 0}
    end.

run() ->
    case version() of
        {Maj, Min} when (Maj > 16 orelse ((Maj == 16) andalso (Min >= 3)))  ->
            run_cover();
        _ ->
            %% older version of cover removes the export_all option
            RetVal = case eunit:test([ts_test_all],
                                     [{report,{eunit_surefire,[{dir,"."}]}}]) of
                         error  -> 1;
                         _ -> 0
                     end,
            init:stop(RetVal)
    end.

run_cover() ->
    {ok, Path} = file:get_cwd(),
    Dir = filename:join(Path,"ebin-test"),
    cover:compile_beam_directory(Dir),
    ModulesAll = cover:modules(),
    Modules = lists:filter(fun(M) -> case atom_to_list(M) of
                                      "ts_test" ++ _ -> false;
                                      "ts_"     ++ _ -> true;
                                      "tsung"   ++ _ -> true;
                                      _              -> false
                                     end
                           end ,  ModulesAll),
    filelib:ensure_dir("cover_report/index.html"),

    RetVal = case eunit:test([ts_test_all], [{report,{eunit_surefire,[{dir,"."}]}}]) of
                 error  -> 1;
                 Result -> 0
             end,
    lists:foreach(fun(M) -> cover:analyse_to_file(M, ?FILE_NAME(M), [html]) end, Modules),
    write_report(lists:sort(Modules)),
    init:stop(RetVal).

test() -> ok.

all_test_() -> [ts_test_recorder,
                ts_test_config,
                ts_test_client,
                ts_test_dynvars_api,
                ts_test_file_server,
                ts_test_options,
                ts_test_pgsql,
                ts_test_proxy,
                ts_test_http,
                ts_test_jabber,
                ts_test_match,
                ts_test_mochi,
                ts_test_mon,
                ts_test_user_server,
                ts_test_search,
                ts_test_stats,
                ts_test_interaction,
                ts_test_websocket,
                ts_test_utils,
                ts_test_mqtt
               ].

%%% The two following functions are copyrighted by:

%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of Erlang Training and Consulting Ltd. nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY Erlang Training and Consulting Ltd. ''AS IS''
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL Erlang Training and Consulting Ltd. BE
%%% LIABLE SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% ----------------------------------------------------------------------------

%%% @author Oscar Hellstrom <oscar@erlang-consulting.com>

write_report(Modules) ->
    {TotalPercentage, ModulesPersentage} = percentage(Modules, 0, 0, []),
    io:format(standard_io,"  Total test coverage: ~p %~n",[TotalPercentage]),
    file:write_file("cover_report/index.html",
        [
            "<html>\n<head><title>Cover report index</title></head>\n"
            "<body>\n"
            "<h1>Cover report for Tsung</h1>"
            "Total coverage: ", integer_to_list(TotalPercentage), "%"
            "<h2>Cover for individual modules</h2>\n"
            "<ul>\n\t",
            lists:foldl(fun({Module, Percentage}, Acc) ->
                        Name = atom_to_list(Module),
                        [
                            "<li>"
                            "<a href=\"", Name ++ ".html" "\">",
                            Name,
                            "</a> ", integer_to_list(Percentage), "%"
                            "</li>\n\t" |
                            Acc
                        ]
                end, [], ModulesPersentage),
            "</ul></body></html>"
        ]).

percentage([Module | Modules], TotCovered, TotLines, Percentages) ->
    {ok, Analasys} = cover:analyse(Module, coverage, line),
    case lists:foldl(fun({_, {C, _}}, {Covered, Lines}) ->
                             {C + Covered, Lines + 1}
                     end, {0, 0}, Analasys)  of
        {_,0} ->
            percentage(Modules, TotCovered, TotLines, Percentages);
        {Covered, Lines} ->
            Percent = (Covered * 100) div Lines,
            NewPercentages = [{Module, Percent} | Percentages],
            percentage(Modules, Covered + TotCovered, Lines + TotLines, NewPercentages)
    end;
percentage([], Covered, Lines, Percentages) ->
    {(Covered * 100) div Lines, Percentages}.



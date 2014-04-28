%%%
%%%  Copyright 2014 Nicolas Niclausse
%%%
%%%  Author : Nicolas Niclausse <nniclaus@sophia.inria.fr>
%%%  Created: 23 avril 2014 by Nicolas Niclausse <nicolas@niclux.org>
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

-module(tsung_web).
-vc('$Id: tsung_web.erl,v 0.0 2014/04/23 12:12:17 nniclaus Exp $ ').
-author('nicolas@niclux.org').

-include("ts_macros.hrl").
-include_lib("kernel/include/file.hrl").

-export([status/3, logs/3, update/3, graph/3, error/3, report/3]).

graph(SessionID, Env, Input) ->
    graph(SessionID, Env, Input,"graph.html").

report(SessionID, Env, Input) ->
    graph(SessionID, Env, Input,"report.html").

graph(SessionID, Env, Input, File) ->
    Begin=now(),
    {ok,Path} = application:get_env(tsung_controller,log_dir_real),
    GraphFile = filename:join(Path,File),
    case update_reports() of
        {error, not_found} ->
            Msg = " Fail to generated reports: tsung_stats.pl was not found in the $PATH or in:<br>  "
                  ++script_paths(),
            error(SessionID, Env, Input, Msg);
        {error, Reason} ->
            error(SessionID, Env, Input, Reason);
        _ ->
            case file:read_file(GraphFile) of
                {error, enoent} ->
                    update(SessionID, Env, Input);
                {ok, Data} ->
                    Time=ts_utils:elapsed(Begin,now()),
                    Text="<div class=\"alert alert-success alert-dismissable\">
  <button type=\"button\" class=\"close\" data-dismiss=\"alert\" aria-hidden=\"true\">\\\&times;</button>
 "++ ts_utils:datestr() ++ ": Report and graphs generated in "++ io_lib:format("~.2f",[Time/1000]) ++" sec
</div>",
                    WorkingDir=filename:basename(Path),
                    Str=replace(Data,[{"=\"style/","=\"/style/"},
                                      {"\"graph.html","\"/tsung/tsung_web:graph"},
                                      {"\"report.html","\"/tsung/tsung_web:report"},
                                      {"csv_data","/csv_data"},
                                      {"<!-- tsung_stats_duration -->",Text},
                                      {"<!-- SUBTITLE -->","Realtime Monitoring - " ++ WorkingDir}
                                     ]),
                    mod_esi:deliver(SessionID,
                                    [ "Content-Type: text/html\r\n\r\n",
                                      Str
                                    ])
            end
    end.

replace(Data,[]) ->
    binary_to_list(iolist_to_binary(Data));
replace(Data,[{Regexp,Replace}|Tail]) ->
    replace(re:replace(Data,Regexp,Replace,[global]), Tail).

error(SessionID, Env, Input) ->
    error(SessionID, Env, Input, "").

error(SessionID, Env, Input, Msg) ->
    Title = "<title>Tsung Update Error</title>",
    Text  = "<div class=\"alert alert-danger \">
  <button type=\"button\" class=\"close\" data-dismiss=\"alert\" aria-hidden=\"true\"></button>
 "++ Msg ++"</div>",

    mod_esi:deliver(SessionID,["Content-Type: text/html\r\n\r\n",
                                head(Title)
                                ++ "<body> "
                                ++ nav()
                                ++ sidebar()
                                ++ " <div class=\"col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main\">"
                                ++ Text
                                ++ foot()
                              ]
                   ).

script_paths()->
    {ok,Path} = application:get_env(tsung_controller,log_dir_real),
    UserPath = filename:join(Path,"../../../lib/tsung/bin"),
    ts_utils:join(":",[UserPath,"/usr/lib64/tsung/bin/","/usr/lib/tsung/bin","/usr/local/lib/tsung/bin"]).

update_reports() ->
    %% Referer = proplists:get_value(http_referer,Env),
    Title ="<title>Tsung Update stats</title>",
    {ok,Path} = application:get_env(tsung_controller,log_dir_real),
    case os:find_executable("tsung_stats.pl") of
        false ->
            case os:find_executable("tsung_stats.pl", script_paths()) of
                false ->
                    {error, not_found};
                RealFile ->
                    Cmd ="cd "++ Path ++ " ; "++ RealFile ++ " --dygraph",
                    os:cmd(Cmd)
            end;
        File ->
            Cmd ="cd "++ Path ++ "; "++ File,
            os:cmd(Cmd)
    end.

update(SessionID, Env, _Input) ->
    Begin=now(),
    Title ="<title>Tsung Update stats</title>",
    update_reports(),
    Time=ts_utils:elapsed(Begin,now()),
    mod_esi:deliver(SessionID, [
                                "Content-Type: text/html\r\n\r\n",
                                head(Title)
                                ++ "<body> "
                                ++ nav()
                                ++ sidebar()
                                ++"<div class=\"container\">"
                                ++ "<div><em>Time to update reports:"++ io_lib:format("~.2f",[Time/1000]) ++" sec </em></div>"
                                ++ foot()
                               ]).


status(SessionID, _Env, _Input) ->
    Title ="<title>Tsung Status</title>",
    {Time, Status} = timer:tc(tsung_controller,status_str,[]),
    mod_esi:deliver(SessionID, [
                                "Content-Type: text/html\r\n\r\n",
                                head(Title)
                                ++ "<body> "
                                ++ nav()
                                ++ sidebar()
                                ++ " <div class=\"col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main\">"
                                ++ "<h1 class=\"page-header\">Status</h1>"

                                ++ "<pre>" ++Status ++ "</pre>"
                                ++ foot()
                               ]).

logs(SessionID, _Env, _Input) ->
    Title ="<title>Tsung Logs</title>",
    {ok,Path} = application:get_env(tsung_controller,log_dir_real),
    {ok,Files} = file:list_dir(Path),
    FilesHTML = lists:map(fun(F)->format(Path,F,"") end,Files),
    mod_esi:deliver(SessionID, [
                                "Content-Type: text/html\r\n\r\n",
                                head(Title)
                                ++ "<body>"
                                ++ nav()
                                ++ sidebar()
                                ++ " <div class=\"col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main\">"

                                ++ "<pre>"++ FilesHTML ++"</pre>"
                                ++ foot()
                               ]).

foot() ->
    VSN = case lists:keysearch(tsung_controller,1,application:loaded_applications()) of
             {value, {_,_ ,V}} -> V;
              _ -> "unknown"
          end,
    "
<footer class=\"bs-docs-footer\" role=\"contentinfo\">
 <div class=\"container\">
  <ul class=\"bs-docs-footer-links muted\">
    <li>Tsung version "++ VSN++"</li>
    <li><address>
        Contact:   <a href=\"mailto:tsung-users@process-one.net\">tsung-users@process-one.net</a>
    </address>
</li>
</ul>
        </div>
</footer>
        </div>
        </div>
        </div>
        </body>
        </html>".

sidebar() ->
  " <div class=\"container-fluid\">
      <div class=\"row\">
         <div class=\"col-sm-3 col-md-2 sidebar\">
           <ul class=\"nav nav-sidebar\">
           </ul>
         </div>
       </div>
    </div>
 ".

head(Title) ->
" <html lang=\"en\">
  <head>
    <meta charset=\"utf-8\">
    <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
    <meta name=\"description\" content=\"\">
    <meta name=\"author\" content=\"\">
    <link rel=\"shortcut icon\" href=\"../../assets/ico/favicon.ico\">

    <title>"++Title ++"</title>

    <link href=\"/style/bootstrap.min.css\" rel=\"stylesheet\">
    <link href=\"/style/dashboard.css\" rel=\"stylesheet\">
    <link href=\"/style/docs.min.css\" rel=\"stylesheet\">

  </head>".

nav() ->
    {ok,Path} = application:get_env(tsung_controller,log_dir_real),
    WorkingDir=filename:basename(Path),
    Subtitle = "Realtime Monitoring - " ++ WorkingDir,
    "
    <div class=\"navbar navbar-inverse navbar-fixed-top\" role=\"navigation\">
      <div class=\"container-fluid\">
        <div class=\"navbar-header\">
          <button type=\"button\" class=\"navbar-toggle\" data-toggle=\"collapse\" data-target=\".navbar-collapse\">
            <span class=\"sr-only\">Toggle navigation</span>
            <span class=\"icon-bar\"></span>
            <span class=\"icon-bar\"></span>
            <span class=\"icon-bar\"></span>
          </button>
          <a class=\"navbar-brand\" href=\"http://tsung.erlang-projects.org/\">Tsung "++ Subtitle++"</a>
        </div>
        <div class=\"navbar-collapse collapse\">
          <ul class=\"nav navbar-nav navbar-right\">
            <li><a href=\"/tsung/tsung_web:status\">Status</a></li>
            <li><a href=\"/tsung/tsung_web:report\">Reports</a></li>
            <li><a href=\"/tsung/tsung_web:graph\">Graphs</a></li>
            <li><a href=\"/tsung/tsung_web:logs\">Logs</a></li>
            <li><a href=\"[% conf %]\">Config</a></li>
          </ul>
        </div>
      </div>
    </div>
".

format(Path,Entry,RequestURI) ->
    case file:read_file_info(filename:join(Path,Entry)) of
        {ok,FileInfo} when FileInfo#file_info.type == directory ->
            {{Year, Month, Day},{Hour, Minute, _Second}} = FileInfo#file_info.mtime,
            EntryLength=length(Entry),
            if
                EntryLength > 21 ->
                    io_lib:format("<strong><A HREF=\"~s\">~-21.s..</A></strong>"
                                  "~2.2.0w-~s-~w ~2.2.0w:~2.2.0w"
                                  "        -\n", [RequestURI++"/"++Entry++"/",
                                                  Entry,
                                                  Day, httpd_util:month(Month),
                                                  Year,Hour,Minute]);
                true ->
                    io_lib:format("<strong><A HREF=\"~s\">~s</A></strong>~*.*c~2.2.0"
                                  "w-~s-~w ~2.2.0w:~2.2.0w        -\n",
                                  [RequestURI ++ "/" ++
                                   Entry ++ "/",Entry,
                                   23-EntryLength,23-EntryLength,$ ,Day,
                                   httpd_util:month(Month),Year,Hour,Minute])
            end;
        {ok,FileInfo} ->
            {{Year, Month, Day},{Hour, Minute,_Second}} = FileInfo#file_info.mtime,
            EntryLength=length(Entry),
            if
                EntryLength > 21 ->
                    io_lib:format(" <A HREF=\"~s\">~-21.s..</A>~2.2.0" ++
                                  "w-~s-~w ~2.2.0w:~2.2.0w~8wk \n",
                                  [RequestURI
                                   ++"/"++Entry, Entry,Day,
                                   httpd_util:month(Month),Year,Hour,Minute,
                                   trunc(FileInfo#file_info.size/1024+1)
                                   ]);
                true ->
                    io_lib:format("<A HREF=\"~s\">~s</A>~*.*c~2.2.0w-~s-~w" ++
                                  " ~2.2.0w:~2.2.0w~8wk \n",
                                  [RequestURI
                                   ++ "/" ++ Entry, Entry, 23-EntryLength,
                                   23-EntryLength, $ ,Day,
                                   httpd_util:month(Month),Year,Hour,Minute,
                                   trunc(FileInfo#file_info.size/1024+1)
                                   ])
            end;
        {error, _Reason} ->
            ""
    end.

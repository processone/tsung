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

-module(ts_web).
-vc('$Id: ts_web.erl,v 0.0 2014/04/23 12:12:17 nniclaus Exp $ ').
-author('nicolas@niclux.org').

-include("ts_macros.hrl").
-include_lib("kernel/include/file.hrl").

-export([start/0, status/3, stop/3, logs/3, update/3, graph/3, error/3, report/3]).

-export([number_to_list/1]).


start() ->
    error_logger:tty(false),
    Redirect= << "<meta http-equiv=\"refresh\" content=\"0; url=/es/ts_web:logs\">\n" >>,
    ts_controller_sup:start_inets(?config(log_dir), Redirect).

graph(SessionID, Env, Input) ->
    graph(SessionID, Env, Input,"graph.html").

report(SessionID, Env, Input) ->
    graph(SessionID, Env, Input,"report.html").

graph(SessionID, Env, Input, File) ->
    Begin=?NOW,
    {ok,Path} = application:get_env(tsung_controller,log_dir_real),
    GraphFile = filename:join(Path,File),
    case update_reports() of
        {error, not_found} ->
            Msg = " Fail to generated reports: tsung_stats.pl was not found in the $PATH or in:<br>  "
                  ++script_paths(),
            error(SessionID, Env, Input, Msg);
        _ ->
            case file:read_file(GraphFile) of
                {error, enoent} ->
                    update(SessionID, Env, Input);
                {ok, Data} ->
                    Time=ts_utils:elapsed(Begin,?NOW),
                    Text="<div class=\"alert alert-success alert-dismissable\">
  <button type=\"button\" class=\"close\" data-dismiss=\"alert\" aria-hidden=\"true\">\\\&times;</button>
 "++ ts_utils:datestr() ++ ": Report and graphs generated in "++ number_to_list(Time/1000) ++" sec
</div>",
                    WorkingDir=filename:basename(Path),
                    Str=replace(Data,[{"=\"style/","=\"/style/"},
                                      {"\"graph.html","\"/es/ts_web:graph"},
                                      {"\"report.html","\"/es/ts_web:report"},
                                      {"csv_data","/csv_data"},
                                      {"<!-- tsung_stats_duration -->",Text},
                                      {"<!-- SUBTITLE -->","Dashboard - " ++ WorkingDir}
                                     ]),
                    mod_esi:deliver(SessionID,
                                    [ "Content-Type: text/html\r\n\r\n",
                                      Str
                                    ])
            end
    end.


error(SessionID, Env, Input) ->
    error(SessionID, Env, Input, "").

error(SessionID, _Env, _Input, Msg) ->
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
            Cmd ="cd "++ Path ++ "; "++ File ++ " --dygraph",
            os:cmd(Cmd)
    end.

update(SessionID, _Env, _Input) ->
    Begin=?NOW,
    Title ="Tsung Update stats",
    update_reports(),
    Time=ts_utils:elapsed(Begin,?NOW),
    mod_esi:deliver(SessionID, [
                                "Content-Type: text/html\r\n\r\n",
                                head(Title)
                                ++ "<body> "
                                ++ nav()
                                ++ sidebar()
                                ++"<div class=\"container\">"
                                ++ "<div><em>Time to update reports:"++ number_to_list(Time/1000) ++" sec </em></div>"
                                ++ foot()
                               ]).

stop(SessionID, _Env, _Input) ->
    Title ="Tsung Stop",
    mod_esi:deliver(SessionID, [
                                "Content-Type: text/html\r\n\r\n",
                                head(Title)
                                ++ "<body> "
                                ++ "<h1 class=\"page-header\">Tsung controller  is stopping now !</h1>"

                               ]),
    slave:stop(node()).

status(SessionID, _Env, _Input) ->
    Title ="Tsung Status",
    {ok, Nodes, Ended_Beams, MaxPhases} = ts_config_server:status(),
    Active    = Nodes - Ended_Beams,
    ActiveBeamsBar  = progress_bar(Active,Nodes,"", "Active nodes: "),
    {Clients, ReqRate, Connected, Interval, Phase, Cpu} = ts_mon:status(),
    NPhase = case Phase of
                 error -> 1;
                 {ok,N} -> (N div Nodes) + 1
             end,
    RequestsBar  = progress_bar(ReqRate/Interval, ReqRate/Interval,"req/sec", lists:flatten("Request rate: ")),
    PhasesBar  = progress_bar(NPhase, MaxPhases,"", lists:flatten("Current phase (total is " ++ number_to_list(MaxPhases) ++" )")),
    UsersBar  = progress_bar(Clients, Clients,"", "Running users"),
    ConnectedBar  = progress_bar(Connected, Clients,"", "Connected users"),
    CPUBar  = progress_bar(Cpu, 100,"", "Controller CPU usage", true),
    mod_esi:deliver(SessionID, [
                                "Content-Type: text/html\r\n\r\n",
                                head(Title)
                                ++ "<body> "
                                ++ nav()
                                ++ sidebar()
                                ++ " <div class=\"col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main\">"
                                ++ "<h1 class=\"page-header\">Status</h1>"

                                ++ UsersBar
                                ++ ConnectedBar
                                ++ RequestsBar
                                ++ ActiveBeamsBar
                                ++ PhasesBar
                                ++ CPUBar
                                ++ foot()
                               ]).

logs(SessionID, _Env, _Input) ->
    Title ="Tsung Logs",
    RealPath = case application:get_env(tsung_controller,log_dir_real) of
                   {ok,Path} -> Path;
                   _         -> ?config(log_dir)
               end,

    {ok,Files} = file:list_dir(RealPath),
    FilesHTML = lists:map(fun(F)->format(RealPath,F,"") end,Files),
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
    Path = case application:get_env(tsung_controller,log_dir_real) of
               {ok,P} -> P;
               _      -> ?config(log_dir)
           end,

    WorkingDir=filename:basename(Path),
    Subtitle = "Dashboard - " ++ WorkingDir,
    "
    <script src=\"/style/jquery.min.js\"></script>
    <script src=\"/style/bootstrap.min.js\"></script>
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
            <li><a href=\"/es/ts_web:status\">Status</a></li>
            <li><a href=\"/es/ts_web:report\">Reports</a></li>
            <li><a href=\"/es/ts_web:graph\">Graphs</a></li>
            <li><a href=\"/es/ts_web:logs\">Logs</a></li>
            <li><a href=\"/es/ts_web:stop\">Stop</a></li>
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


%% helper functions

number_to_list(F) when is_integer(F)-> integer_to_list(F);
number_to_list(F) -> io_lib:format("~.2f",[F]).

replace(Data,[]) ->
    binary_to_list(iolist_to_binary(Data));
replace(Data,[{Regexp,Replace}|Tail]) ->
    replace(re:replace(Data,Regexp,Replace,[global]), Tail).

progress_bar(Val, Max, Unit, Title) ->
    progress_bar(Val, Max, Unit, Title, false).

progress_bar(Val, Max, Unit, Title, Variable) ->
    Percent = case Max of
                  0 -> 0;
                  0.0 -> 0;
                  M -> round(100 * Val / M)
              end,
    ProgressType = if
                       Variable == false ->
                           "progress-bar-success";
                       Percent > 80 ->
                           "progress-bar-danger";
                       Percent > 60 ->
                           "progress-bar-warning";
                       Percent > 40 ->
                           "progress-bar-info";
                       true->
                           "progress-bar-success"
                   end,

    Title ++ "<div class=\"progress\">"
        ++ "<div class=\"progress-bar "++ ProgressType++ "\" role=\"progressbar\" aria-valuenow=\""
        ++ number_to_list(Val)
        ++ "\" aria-valuemin=\"0\" aria-valuemax=\""
        ++ number_to_list(Max)
        ++ "\" style=\"width: "
        ++ number_to_list(Percent)
        ++ "%;\">"
        ++ number_to_list(Val)
        ++ " " ++Unit
        ++"</div> </div>".


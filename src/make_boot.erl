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

-module(make_boot).
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-export([make_boot/1]).


make_boot([App]) ->
    A = atom_to_list(App),
    case catch systools:make_script(A,[ no_module_tests,
									   {variables,[{"TSUNAMIPATH","./ebin"}]}]
								   ) of
        ok ->
            halt();
        {'EXIT', Error} ->
            io:format("Error = ~p~n", [Error]),
            halt(1);
        E ->
            io:format("~p~n", [E]),
            halt(1)
    end.


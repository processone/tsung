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

-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-record(jabber, {dest, size, type, jud_param, cle, id = 0}).

-define(jabber_users, ts_utils:get_val(jabber_users)).
-define(jabber_domain, ts_utils:get_val(jabber_domain)).
-define(jabber_server, ts_utils:get_val(jabber_server)).
-define(jabber_username, ts_utils:get_val(jabber_username)).
-define(jabber_password, ts_utils:get_val(jabber_password)).
-define(n_roster_clients, ts_utils:get_val(n_roster_clients)).
-define(setroster_intensity, 1/(ts_utils:get_val(setroster)*1000)).
-define(req_filename, ts_utils:get_val(req_filename)).
-define(presence_delay, ts_utils:get_val(presence_delay)).
-define(boucle1_percent, ts_utils:get_val(boucle1_percent)).
-define(boucle2_percent, ts_utils:get_val(boucle2_percent)).
-define(boucle3_percent, ts_utils:get_val(boucle3_percent)).

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

%%% Random Generators for several probability distributions 

-module(ts_stats).
-created('Date: 2000/10/20 13:58:56 nniclausse Exp ').
-vc('$Id$ ').
-author('nicolas.niclausse@IDEALX.com').

-export([exponential/1, exponential/2, pareto/2, mean/1]).

-record(pareto, {a = 1 , beta}).

%% get n samples from a function F with parameter Param
sample (F, Param, N) ->
    sample(F, [], Param, N-1).

sample (F, X, Param, 0) ->
    [F(Param)] ++ X ;
sample (F, X, Param, N) ->
    sample(F, X ++ [F(Param)], Param, N-1 ).
    
%% random sample from an exponential distribution
exponential(Param) ->
    -math:log(random:uniform())/Param.

%% N samples from an exponential distribution
exponential(Param, N) ->
    {Msec, Sec, Nsec} = now(), 
    random:seed(Msec,Sec,Nsec), % est-ce necessaire de faire ça ici ?
    sample(fun(X) -> exponential(X) end , Param, N).

%% random sample from a Pareto distribution
pareto(Param) ->
    Param#pareto.a/(math:pow(random:uniform(), 1/Param#pareto.beta)).

%% if a list is given, construct a record for the parameters
pareto(Param, N) when list(Param)->
    pareto(#pareto{a = lists:nth(1,Param) , beta = lists:nth(2,Param) }, N);
%% N samples from a Pareto distribution
pareto(Param, N) ->
    random:seed(), % est-ce necessaire de faire ça ici ?
    sample(fun(X) -> pareto(X) end , Param, N).

%% use to compute the mean of a list
mean(N, []) -> N;

mean(N, [X|H]) ->
    I = length(H) + 1,
    mean((X+N*I)/(I+1), H).

%% compute the mean of a list
mean([]) -> 0;

mean([X|H]) ->
    mean(X, H).

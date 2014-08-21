%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Procedures to sort the arcs in the diagram
%%% @end
%%% Created : 31 Jul 2014 by Pablo Lamela Seijas
%%%-------------------------------------------------------------------
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above
%%% copyright notice, this list of conditions and the following
%%% disclaimer in the documentation and/or other materials provided
%%% with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived
%%% from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
-module(arc_sorter).

-include("records.hrl").

-export([sort_arcs/1]).

sort_arcs(ArcsList) ->
    Grouped = utils:group_by(fun (#diagram_arc{id_start = From}) -> From end, ArcsList),
    SortedGroups = lists:map(fun sort_group/1, Grouped),
    lists:concat(SortedGroups).

sort_group([#diagram_arc{tags = Tag}] = SingleArc) when length(Tag) < 2 -> SingleArc;
sort_group(Arcs) ->
    try lists:sort(fun compare_arcs_exceptional/2, Arcs)
    catch throw:incompatible -> paint_red(lists:sort(fun compare_arcs/2, Arcs))
    end.

paint_red([]) -> [];
paint_red([(#diagram_arc{properties = Opts} = Arc)|Rest]) ->
    [Arc#diagram_arc{properties = [{color, 80, 0, 0}|Opts]}|paint_red(Rest)].

compare_arcs_exceptional(Arc1, Arc2) ->
    compare_arcs_aux(Arc1, Arc2, true).

compare_arcs(Arc1, Arc2) ->
    compare_arcs_aux(Arc1, Arc2, false).

compare_arcs_aux(#diagram_arc{tags = Tags1},
		 #diagram_arc{tags = Tags2},
		 IsExceptional) ->
    FTags1 = lists:sort(lists:map(fun collapse_on_el1/1, fil_and_org_tags(Tags1))),
    FTags2 = lists:sort(lists:map(fun collapse_on_el1/1, fil_and_org_tags(Tags2))),
    Minimal1 = minId(Tags1),
    Minimal2 = minId(Tags2),
    IsMin1Smaller = (Minimal1 =< Minimal2),
    Sortings = find_sortings(FTags1, FTags2),
    case summarise(Sortings) of
	empty -> IsMin1Smaller;
	undefined -> IsMin1Smaller;
	bigger -> false;
	smaller -> true;
	contradictory -> case IsExceptional of
			     true -> throw(incompatible);
			     false -> IsMin1Smaller
			 end
    end.

collapse_on_el1([{Elem, _}|_] = List) -> {Elem, collapse_on_el1_aux(Elem, List)}.
collapse_on_el1_aux(Elem, [{Elem, SElem}|Rest]) ->
    [SElem|collapse_on_el1_aux(Elem, Rest)];
collapse_on_el1_aux(_, []) -> [].

fil_and_org_tags(Tags) ->
    utils:group_by(fun ({Ori, _}) -> Ori end,
		   [{Ori, Id} || {historic_rel, Ori, Id, _} <- Tags]).

find_sortings([{O, List1}|Rest], [{O, List2}|Rest2]) ->
    [compare(List1, List2)|find_sortings(Rest, Rest2)];
find_sortings([{O1, _}|_] = Rest1, [{O2, _}|Rest2]) when O1 > O2 ->
    find_sortings(Rest1, Rest2);
find_sortings([{O1, _}|Rest1], [{O2, _}|_] = Rest2) when O2 > O1 ->
    find_sortings(Rest1, Rest2);
find_sortings(_, _) -> [].

compare(List1, List2) ->
    case {(lists:min(List1) < lists:min(List2) andalso
	   lists:max(List1) < lists:max(List2)),
	  (lists:min(List2) < lists:min(List1) andalso
	   lists:max(List2) < lists:max(List1))} of
	{true, false} -> smaller;
	{false, true} -> bigger;
	{_, _} -> undefined
    end.

summarise(List) -> summarise(List, empty).
summarise([], Sth) -> Sth;
summarise([undefined|Rest], Sth) -> summarise(Rest, Sth);
summarise([Sth|Rest], empty) -> summarise(Rest, Sth);
summarise([bigger|_], smaller) -> contradictory;
summarise([smaller|_], bigger) -> contradictory;
summarise([Dec|Rest], Dec) -> summarise(Rest, Dec).

minId(Tags) -> lists:min([Id || {historic_rel, _, Id, _} <- Tags]).



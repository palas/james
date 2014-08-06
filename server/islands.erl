%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Separates a diagram into islands
%%% @end
%%% Created : 27 Jun 2014 by Pablo Lamela Seijas
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
-module(islands).

-export([get_islands/2, get_biggest_islands/2, fusion_islands/1]).

-include("records.hrl").


get_islands(Nodes, Arcs) ->
    SetsList = get_island_sets(lists:map(fun dia_utils:get_nod_id/1, Nodes),
			       dia_utils:create_drai(Nodes, Arcs)),
    SepNodes = utils:separate_by(fun dia_utils:get_nod_id/1, SetsList, Nodes),
    SepArcs = utils:separate_by(fun dia_utils:get_arc_from/1, SetsList, Arcs),
    [dia_utils:create_drai(SN, SA) || {SN, SA} <- lists:zip(SepNodes, SepArcs)].

% Returns only the N biggest islands at most
get_biggest_islands(N, Islands) ->
    case ((N =/= inf) andalso (length(Islands) > N)) of
	true -> element(1, lists:split(N, sort_islands(Islands)));
	false -> sort_islands(Islands)
    end.

sort_islands(Islands) ->
    element(2, lists:unzip(lists:reverse(lists:sort([{dict:size(Island#drai.dnodes), Island} || Island <- Islands])))).

% Merges islands into a tuple of nodes and arcs
fusion_islands(Islands) ->
    {FNodes, FArcs} = lists:unzip([{utils:dic_values(NNodes), utils:dic_values(NArcs)}
				   || #drai{dnodes = NNodes, darcs = NArcs} <- Islands]),
    {lists:concat(FNodes), lists:concat(FArcs)}.

%% Diagram level functions
%% =======================

get_island_sets(NodeIdList, Drai) ->
    get_island_sets_aux(NodeIdList, Drai, []).
get_island_sets_aux([], _, SetList) -> SetList;
get_island_sets_aux([Node|Rest], Drai, SetList) ->
    case utils:is_element_of_any_set(Node, SetList) of
	true -> get_island_sets_aux(Rest, Drai, SetList);
	false ->
	    NewSet = expand_node(Node, Drai),
	    get_island_sets_aux(Rest, Drai, [NewSet|SetList])
    end.

% Generate a set of all the reachable nodes
% from Node, using the Drai record
expand_node(Node, Drai) ->
    expand_nodes(sets:new(), sets:from_list([Node]), Drai).

expand_nodes(FixedNodes, NodeSet, Drai) ->
    GeneratedNodeSet = dia_utils:expand_nodes_once(NodeSet, Drai),
    NewFixedNodes = sets:union(FixedNodes, NodeSet),
    NewNodeSet = sets:subtract(GeneratedNodeSet, NewFixedNodes),
    case sets:size(NewNodeSet) of
	0 -> NewFixedNodes;
	_ -> expand_nodes(NewFixedNodes, NewNodeSet, Drai)
    end.


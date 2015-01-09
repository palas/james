%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Low level helper functions for diagams
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
-module(dia_utils).

-include("records.hrl").

-export([expand_nodes_once/2, get_arc_from/1, get_nod_id/1, create_drai/2,
	 generate_diamonds_in_drai/1, get_normal_nodes/1, rebuild_idxs/1,
	 remove_duplicated_arcs/1, remove_orphan_nodes/1, remove_up_from/2,
	 expand_node_id_to_trans_up/2, expand_node_id_to_trans_down/2,
	 expand_tran_upwards/2, expand_tran_downwards/2, get_nod_ids/1,
	 get_node_prop/1, get_tran_prop/1, join_node_pairs/3, sort_trans/1,
	 sort_trans_desc/2, collapse_integers/1, highlight_loops/1,
	 remove_elliptic_nodes/1, expand_nodes_down/2, expand_nodes_up/2,
	 get_node_by_id/2, set_node/2, set_arc/2, remove_node/2,
	 move_returns/3, get_arcs_up/2, generate_subgraphs/1, print_nodeids/1,
	 expand_nodes_within_cluster/2, resolve_ids/2, get_cluster_id/1,
	 expand_diamonds_down/2, is_data_dep/1, get_top_nodes/1,
	 get_control_nodes/1]).

%% Low level diagram record interface functions
%% ============================================

create_drai(Nodes, Arcs) ->
    DNodes = nodes_to_dict(Nodes),
    DArcs = arcs_to_dict(Arcs),
    rebuild_idxs(#drai{dnodes = DNodes,
		       darcs = DArcs}).

get_nod_id(#diagram_node{id = Id}) -> Id.
get_nod_ids(List) -> lists:map(fun get_nod_id/1, List).
get_arc_id(#diagram_arc{id = Id}) -> Id.
get_arc_from(#diagram_arc{id_start = Id}) -> Id.
get_arc_to(#diagram_arc{id_end = Id}) -> Id.


mk_filter_plus_to(Drai, Filter) ->
	fun (Id, Set) -> filter_plus_to(Filter, dict:fetch(Id, Drai#drai.darcs), Set) end.

filter_plus_to(Filter, Arc, Set) ->
	case Filter(Arc) of
		true -> sets:add_element(get_arc_to(Arc), Set);
		false -> Set
	end.

is_data_dep(#diagram_arc{content = this}) -> true;
is_data_dep(#diagram_arc{content = {param, _}}) -> true;
is_data_dep(_) -> false.

get_node_by_id(NodeId, #drai{dnodes = DNodes}) -> dict:find(NodeId, DNodes).
get_nodes_by_ids([], _Drai) -> [];
get_nodes_by_ids([NodeId|Rest], Drai) ->
    [dict:fetch(NodeId, Drai#drai.dnodes)|get_nodes_by_ids(Rest, Drai)].

create_arc_solver(Fun, Drai) -> fun (X) -> arc_solve(Fun, X, Drai) end.

arc_solve(Fun, Id, Drai) -> Fun(dict:fetch(Id, Drai#drai.darcs)).

expand_nodes_once(NodeSet, Drai) ->
    NodeSetFrom = expand_nodes_down(NodeSet, Drai),
    NodeSetTo = expand_nodes_up(NodeSet, Drai),
    sets:union(NodeSetFrom, NodeSetTo).

expand_nodes_down_wt_arcfilter(Filter, NodeSet, #drai{arcsf = FromTo} = Drai) ->
	sets:fold(mk_filter_plus_to(Drai, Filter), sets:new(),
		utils:expand_set_through_idx(NodeSet, FromTo)).

expand_nodes_down(NodeSet, #drai{arcsf = FromTo} = Drai) ->
    utils:sets_map(create_arc_solver(fun get_arc_to/1, Drai),
		   utils:expand_set_through_idx(NodeSet, FromTo)).

% Expands diamond nodes down but lets the others where they are
expand_diamonds_down(Drai, NodeSet) ->
	{Norm, Dia} = split_diamonds(Drai, NodeSet),
	sets:union(Norm, expand_nodes_down(Dia, Drai)).

split_diamonds(#drai{dnodes = DNodes}, NodeSet) ->
	sets:fold(split_diamonds_fun_aux(DNodes), {sets:new(), sets:new()}, NodeSet).
split_diamonds_fun_aux(DNodes) ->
	fun (NodeId, {Norm, Dia}) ->
		#diagram_node{tags = Tags} = dict:fetch(NodeId, DNodes),
		case lists:member(diamond, Tags) of
			true -> {Norm, sets:add_element(NodeId, Dia)};
			false -> {sets:add_element(NodeId, Norm), Dia}
		end
	end.

expand_nodes_up(NodeSet, #drai{arcst = ToFrom} = Drai) ->
    utils:sets_map(create_arc_solver(fun get_arc_from/1, Drai),
		   utils:expand_set_through_idx(NodeSet, ToFrom)).

get_arcs_up(NodeSet, #drai{arcst = ToFrom} = Drai) ->
    utils:sets_map(create_arc_solver(fun utils:id/1, Drai),
		   utils:expand_set_through_idx(NodeSet, ToFrom)).
get_arcs_down(NodeSet, #drai{arcsf = FromTo} = Drai) ->
    utils:sets_map(create_arc_solver(fun utils:id/1, Drai),
		   utils:expand_set_through_idx(NodeSet, FromTo)).

get_arcs_ids_up(NodeSet, #drai{arcst = ToFrom} = Drai) ->
    utils:sets_map(create_arc_solver(fun get_arc_id/1, Drai),
		   utils:expand_set_through_idx(NodeSet, ToFrom)).

get_arcs_ids_down(NodeSet, #drai{arcsf = FromTo} = Drai) ->
    utils:sets_map(create_arc_solver(fun get_arc_id/1, Drai),
		   utils:expand_set_through_idx(NodeSet, FromTo)).

rebuild_idxs(#drai{darcs = DArcs} = Drai) ->
    Drai#drai{arcsf = utils:mk_idx(fun get_arc_from/1, DArcs),
	      arcst = utils:mk_idx(fun get_arc_to/1, DArcs)}.

arcs_to_dict(Arcs) -> utils:mk_dic(fun get_arc_id/1, Arcs).
nodes_to_dict(Nodes) -> utils:mk_dic(fun get_nod_id/1, Nodes).

remove_duplicated_arcs(#drai{
			  darcs = DArcs
			 } = Drai) ->
    Drai#drai{darcs = remove_duplicated_arcs(DArcs)};
remove_duplicated_arcs(DArcs) ->
    element(2, dict:fold(fun dup_arc_fold/3,
			 {dict:new(), dict:new()},
			 DArcs)).

get_cluster_id(#diagram_node{cluster = {cluster, Id, _}}) -> Id;
get_cluster_id(_) -> none.

dup_arc_fold(_, #diagram_arc{
		   id = Id,
		   id_start = IdS,
		   id_end = IdE,
		   tags = Tags} = Arc,
	     {Dict, DArcs}) ->
    Entry = {IdS, IdE, get_tran_prop(Arc)},
    case dict:find(Entry, Dict) of
	{ok, #diagram_arc{id = OldId, tags = OldTags} = Value} ->
	    ModifiedValue = Value#diagram_arc{
			      tags = Tags ++ OldTags},
            {dict:store(Entry, ModifiedValue, Dict),
	     dict:store(OldId, ModifiedValue, DArcs)};
	error -> {dict:store(Entry, Arc, Dict),
		  dict:store(Id, Arc, DArcs)}
    end.

get_tran_prop(#diagram_arc{properties = Prop,
			   content = Content}) ->
    {Prop, Content}.

get_tran_prop_and_dmn(#diagram_arc{id_end = DestId,
				   properties = Prop,
				   content = Content},
		      #drai{dnodes = DNodes}) ->
    {Prop, get_node_prop(dict:fetch(DestId, DNodes)), Content}.

get_node_prop(#diagram_node{label = Label, cluster = Cluster, class = Class}) -> {Label, Cluster, Class}.

generate_diamonds_in_drai(#drai{dnodes = DNodes,
		                darcs = DArcs} = Drai) ->
    ListOfLists = element(2, lists:unzip(group_arcs_dict_by_to_nodeid(DArcs))),
    Mixture = lists:flatten(lists:map(fun collapse_arcs_to_diamonds/1, ListOfLists)),
    NewArcs = [Arc || #diagram_arc{} = Arc <- Mixture],
    NewNodes = [Node || #diagram_node{} = Node <- Mixture],
    Drai#drai{dnodes = dict:merge(fun (_, _) -> throw(key_conflict) end,
				  DNodes, nodes_to_dict(NewNodes)),
	      darcs = arcs_to_dict(NewArcs)}.

group_arcs_dict_by_to_nodeid(DArcs) ->
    dict:to_list(dict:fold(fun get_node_arc_pairs_fold/3, dict:new(), DArcs)).

get_node_arc_pairs_fold(_, Arc, Acc) ->
    dict:append(get_arc_to(Arc), Arc, Acc).

collapse_arcs_to_diamonds(List) ->
    GroupedList = utils:group_by(fun compare_arcs/1, List),
    lists:map(fun add_diamond_if_several/1, GroupedList).

compare_arcs(A) -> get_tran_prop(A).

add_diamond_if_several([A]) -> [A];
add_diamond_if_several(Several) ->
    DiaId = "diamond" ++ get_arc_to(hd(Several)) ++ "0"
	++ get_arc_id(hd(Several)),
    [#diagram_node{id = DiaId, label = "oneOf", properties = [diamond]},
     #diagram_arc{id = "trans" ++ DiaId, id_start = DiaId,
		  id_end = get_arc_to(hd(Several)),
		  content = (hd(Several))#diagram_arc.content,
		  properties = (hd(Several))#diagram_arc.properties}
     |lists:map(fun (X) -> change_dest_to(DiaId, X) end, Several)].

change_dest_to(End, #diagram_arc{} = Arc) ->
    Arc#diagram_arc{id_end = End}.

remove_node(NodeId, #drai{
		       dnodes = DNodes
		      } = Drai) ->
    NewDNodes = dict:erase(NodeId, DNodes),
    NodeIdSet = sets:from_list([NodeId]),
    ArcsUpIdSet = get_arcs_ids_up(NodeIdSet, Drai),
    ArcsDownIdSet = get_arcs_ids_down(NodeIdSet, Drai),
    NewDrai = lists:foldl(fun remove_arc/2, Drai, sets:to_list(sets:union(ArcsUpIdSet, ArcsDownIdSet))),
    NewDrai#drai{dnodes = NewDNodes}.

remove_arc(ArcId,
	   #drai{
	      darcs = Darcs,
	      arcsf = FromTo,
	      arcst = ToFrom
	     } = Drai) ->
    case dict:find(ArcId, Darcs) of
	{ok, (#diagram_arc{id = Id, id_start = IdStart, id_end = IdEnd})} ->
	    begin
		NewDarcs = dict:erase(Id, Darcs),
		NewFromTo = case dict:find(IdStart, FromTo) of
				{ok, Value1} -> dict:store(IdStart, sets:del_element(Id, Value1), FromTo);
				error -> ok
			    end,
		NewToFrom = case dict:find(IdEnd, ToFrom) of
				{ok, Value2} -> dict:store(IdEnd, sets:del_element(Id, Value2), ToFrom);
				error -> ok
			    end,
		Drai#drai{
		  darcs = NewDarcs,
		  arcsf = NewFromTo,
		  arcst = NewToFrom
		 }
	    end;
	error -> Drai
    end.

move_returns(NodeOriId, NodeDestId, Drai) ->
    NodeIdSet = sets:from_list([NodeOriId]),
    ArcsDownIdSet = get_arcs_ids_down(NodeIdSet, Drai),
    lists:foldl(fun (X, Acc) ->
			change_arc_ori(NodeDestId, X, Acc)
		end, move_arcs_in_f_index(NodeOriId, NodeDestId, Drai),
		sets:to_list(ArcsDownIdSet)).

move_arcs_in_f_index(NodeOriId, NodeDestId,
		     #drai{
			arcsf = FromTo
		       } = Drai) ->
    NewSet = sets:union(
	       utils:dict_def_find(sets:new(), NodeOriId, FromTo),
	       utils:dict_def_find(sets:new(), NodeDestId, FromTo)
	      ),
    FromTo2 = dict:erase(NodeOriId, FromTo),
    NewFromTo = dict:store(NodeDestId, NewSet, FromTo2),
    Drai#drai{
      arcsf = NewFromTo
     }.


change_arc_ori(NodeDestId, ArcId,
	       #drai{
		  darcs = Darcs
		 } = Drai) ->
    Drai#drai{
      darcs = dict:store(ArcId,
			 case dict:find(ArcId, Darcs) of
			     {ok, ArcAsWas} -> ArcAsWas#diagram_arc{id_start = NodeDestId};
			     error -> Drai
			 end, Darcs)
     }.

remove_up_from(List, Drai) ->
    Nodes = find_nodes(sets:from_list(List), Drai),
    {ExNodes, TempArcs} = expand_up_while_possible(Nodes, Drai),
    ExArcs = sets:union(TempArcs, get_arcs_ids_down(Nodes, Drai)),
    rebuild_idxs(remove_arcs(ExArcs, remove_nodes(ExNodes, Drai))).

find_nodes(TagSet, #drai{dnodes = DNodes}) ->
    dict:fold(fun (_, V, Set) ->
		      find_nod_fold(V, Set, TagSet)
	      end, sets:new(), DNodes).
remove_arcs(ArcIdSet, #drai{darcs = DArcs} = Drai) ->
    Drai#drai{
      darcs =
	  dict:fold(fun (_, V, Dic) ->
			    rem_arc_fold(V, Dic, ArcIdSet)
		    end, dict:new(), DArcs)}.
remove_nodes(NodeIdSet, #drai{dnodes = DNodes} = Drai) ->
    Drai#drai{
      dnodes =
	  dict:fold(fun (_, V, Dic) ->
			    rem_nod_fold(V, Dic, NodeIdSet)
		    end, dict:new(), DNodes)}.

find_nod_fold(#diagram_node{id = Id,
			    label = Label}, Set, TagSet) ->
    case sets:is_element(Label, TagSet) of
	true -> sets:add_element(Id, Set);
	false -> Set
    end.
rem_arc_fold(#diagram_arc{id = Id} = Arc, Dic, ArcIdList) ->
    case sets:is_element(Id, ArcIdList) of
	true -> Dic;
	false -> dict:store(Id, Arc, Dic)
    end.
rem_nod_fold(#diagram_node{id = Id} = Node, Dic, NodeIdList) ->
    case sets:is_element(Id, NodeIdList) of
	true -> Dic;
	false -> dict:store(Id, Node, Dic)
    end.

expand_up_while_possible(Nodes, Drai) ->
    expand_up_while_possible(Nodes, sets:new(), sets:new(), Drai).
expand_up_while_possible(Nodes, OldNodes, OldArcs, Drai) ->
    NewNodes = expand_nodes_up(Nodes, Drai),
    NewArcs = get_arcs_ids_up(Nodes, Drai),
    NextOldNodes = sets:union(OldNodes, Nodes),
    NextOldArcs = sets:union(OldArcs, NewArcs),
    case sets:size(NextOldNodes) =:= sets:size(OldNodes) of
	true -> {NextOldNodes, NextOldArcs};
	false -> expand_up_while_possible(NewNodes, NextOldNodes,
					  NextOldArcs, Drai)
    end.


%% Returns a list of nodes from a drai, eventually
%% filtering "oneof" nodes and so
get_normal_nodes(#drai{dnodes = DNodes}) -> utils:dic_values(DNodes).

join_node_pairs(NodeId1, NodeId2,
	       #drai{
		  dnodes = DNodes,
		  darcs = DArcs
	         } = Drai) ->
    %% io:format("Merging: ~p ~p~n", [NodeId1, NodeId2]),
    NewDArcs = dict:map(
		 fun (_, Val) ->
			 replace_nodeid_in_arc(Val,
				               NodeId2,
				               NodeId1)
		 end, DArcs),
    Node1 = dict:fetch(NodeId1, DNodes),
    NewDNodes = dict:store(NodeId1,
			   Node1#diagram_node{tags = lists:usort(Node1#diagram_node.tags
								 ++ (dict:fetch(NodeId2, DNodes))#diagram_node.tags)
					     }, DNodes),
    Drai#drai{
      dnodes = NewDNodes,
      darcs = NewDArcs
     }.

replace_nodeid_in_arc(Arc, NodeId, NodeId) -> Arc;
replace_nodeid_in_arc(#diagram_arc{
	                 id_start = NodeId1
	                } = Arc,
	              NodeId1, NodeId2) ->
    replace_nodeid_in_arc(Arc#diagram_arc{
		            id_start = NodeId2
	                   }, NodeId1, NodeId2);
replace_nodeid_in_arc(#diagram_arc{
	                 id_end = NodeId1
	                } = Arc,
	              NodeId1, NodeId2) ->
    replace_nodeid_in_arc(Arc#diagram_arc{
		            id_end = NodeId2
	                   }, NodeId1, NodeId2);
replace_nodeid_in_arc(Else, _, _) -> Else.

remove_orphan_nodes(#drai{
		      dnodes = DNodes,
		      darcs = DArcs} = Drai) ->
    NonOrfanSet = dict:fold(fun (_, V, S) ->
				    find_non_orphan(V, S)
			    end,
			    sets:new(),
			    DArcs),
    NewDNodes = dict:fold(fun (K, V, D) ->
				  store_non_orphan({K, V}, NonOrfanSet, D)
			  end,
			  dict:new(),
			  DNodes),
    Drai#drai{dnodes = NewDNodes}.

find_non_orphan(#diagram_arc{id_start = Id1,
			    id_end = Id2}, Set) ->
    sets:add_element(Id1, sets:add_element(Id2 , Set)).

store_non_orphan({NodeId, Node}, NonOrfanSet, DNodes) ->
    case sets:is_element(NodeId, NonOrfanSet) of
	true ->
	    dict:store(NodeId, Node, DNodes);
	false ->
	    %% io:format("Removing orphan: ~p~n", [NodeId]),
	    DNodes
    end.

% Takes an arc record and finds the node records that
% are upwards (where the arcs start)
expand_tran_upwards(Tran, #drai{dnodes = DNodes}) ->
    From = get_arc_from(Tran),
    dict:fetch(From, DNodes).

% Takes an arc record and finds the node records that
% are upwards (where the arcs start)
expand_tran_downwards(Tran, #drai{dnodes = DNodes}) ->
    From = get_arc_to(Tran),
    dict:fetch(From, DNodes).

% Takes a list of arc records and sorts it using its label
sort_trans(TranList) ->
    utils:usort_using(fun get_tran_prop/1, TranList).

% It is like sort_trans/1 but considers the method name of the dest node
sort_trans_desc(TranList, Island) ->
    utils:usort_using(fun (X) -> get_tran_prop_and_dmn(X, Island) end,
		      TranList).

% Takes a NodeId and extracts all the upward arcs as records
expand_node_id_to_trans_up(NodeId, Drai) ->
    NodeSet = sets:from_list([NodeId]),
    sets:to_list(
      get_arcs_up(NodeSet, Drai)).

% Takes a NodeId and extracts all the downward arcs as records
expand_node_id_to_trans_down(NodeId, Drai) ->
    NodeSet = sets:from_list([NodeId]),
    sets:to_list(
      get_arcs_down(NodeSet, Drai)).

remove_elliptic_nodes(#drai{dnodes = DNodes} = Drai) ->
    remove_orphan_arcs(Drai#drai{
			dnodes = dict:filter(
				   fun (_, #diagram_node{properties = Properties}) ->
					   [] =:= [1 || ellipse <- Properties]
				   end, DNodes)}).

print_nodeids(#drai{dnodes = DNodes} = Drai) ->
    Drai#drai{dnodes = dict:map(
			 fun (_, #diagram_node{id = Id, label = Label} = Node) when is_list(Label) ->
				 Node#diagram_node{label = Id ++ " - " ++ Label};
				   (_, #diagram_node{id = Id, label = Label} = Node) ->
				 Node#diagram_node{label = Id ++ " - " ++ lists:flatten(io_lib:format("~p", [Label]))}
			 end, DNodes)}.

%% Collapse integers
%% =================

collapse_integers(Drai) ->
    ListOfListsOfNodes = get_node_lists_to_diamond(Drai),
    lists:foldl(fun collapse_integer_aux/2, Drai, ListOfListsOfNodes).

collapse_integer_aux({DiaId, ListOfNodes}, Drai) ->
    {UnsortedInts, _} = lists:partition(fun isIntegerNode/1, ListOfNodes),
    Ints = utils:sort_using(fun (X) -> element(2, element(1,get_node_prop(X))) end, UnsortedInts),
    NewInts = special_zip(
		utils:sort_using(fun (X) -> element(2, X) end,
				 integer_list:collapse_integer_list(
				   lists:map(fun (X) -> element(1, get_node_prop(X)) end, Ints))),
		Ints),
    RenamedNewInts = lists:map(fun add_tag_to_node_id/1, NewInts),
    Drai2 = update_nodes(RenamedNewInts, Drai),
    Drai3 = remove_or_tag_arcs_between(sets:from_list(get_nod_ids(Ints)),
				       sets:from_list(get_nod_ids(NewInts)),
				       DiaId, Drai2),
    remove_orphan_nodes(Drai3).

special_zip([], []) -> [];
special_zip([({integer, _} = NewProp)|Rest], [El|Rest2]) ->
    io:format("integer\n"),
    [set_node_prop(NewProp, El)|special_zip(Rest, Rest2)];
special_zip([({integer_range, X, Y} = NewProp)|Rest], [(#diagram_node{tags = Tags} = El)|Rest2]) ->
    io:format("integer_range\n"),
    {RElems, NewRest2} = lists:split(Y - X, Rest2),
    [set_node_prop(NewProp, El#diagram_node{tags = lists:usort(Tags ++ get_tags(RElems))})
     |special_zip(Rest, NewRest2)].

get_tags([]) -> [];
get_tags([#diagram_node{tags = Tags}|Rest]) -> Tags ++ get_tags(Rest).

isIntegerNode(Node) ->
    case element(1, get_node_prop(Node)) of
	{integer, _} -> true;
	_ -> false
    end.

add_tag_to_node_id(Node) ->
    Node#diagram_node{id = add_tag_to_id(get_nod_id(Node))}.
add_tag_to_arc_from_id(Arc) ->
    Arc#diagram_arc{id_start = add_tag_to_id(get_arc_from(Arc))}.

add_tag_to_id(OriginalId) -> "ir" ++ OriginalId.

set_node_prop(Label, #diagram_node{} = Node) -> Node#diagram_node{label = Label}.

set_node(#diagram_node{id = Id} = Node, #drai{dnodes = DNodes} = Drai) ->
    Drai#drai{dnodes = dict:store(Id, Node, DNodes)}.

set_arc(#diagram_arc{id = Id} = Arc, #drai{darcs = DArcs} = Drai) ->
    Drai#drai{darcs = dict:store(Id, Arc, DArcs)}.

%% lazy_zip_with(Fun, [H1|T1], [H2|T2]) -> [Fun(H1, H2)|lazy_zip_with(Fun, T1, T2)];
%% lazy_zip_with(_, _, _) -> [].

update_nodes(NewNodes, #drai{dnodes = DNodes} = Drai) ->
    Drai#drai{dnodes = lists:foldl(fun (X, Y) -> dict:store(get_nod_id(X), X, Y) end,
				  DNodes, NewNodes)}.

update_arcs(NewArcs, #drai{darcs = DArcs} = Drai) ->
    Drai#drai{darcs = lists:foldl(fun (X, Y) -> dict:store(get_arc_id(X), X, Y) end,
				   DArcs, NewArcs)}.

remove_or_tag_arcs_between(SetNodesOri, SetNodesToRenameOri, NodeDest,
			   #drai{darcs = DArcs} = Drai) ->
    Drai#drai{darcs = element(1, dict:fold(fun remove_or_tag_arcs_between_fold/3,
					   {dict:new(), SetNodesOri,
					    SetNodesToRenameOri, NodeDest}, DArcs))}.
remove_or_tag_arcs_between_fold(Id, #diagram_arc{id_start = NodeOri,
						 id_end = NodeDest} = Arc,
				{Dict, SetNodesOri, SetNodesToRenameOri, NodeDest}) ->
    case {sets:is_element(NodeOri, SetNodesOri),
	  sets:is_element(NodeOri, SetNodesToRenameOri)} of
	{true, true} -> {dict:store(Id, add_tag_to_arc_from_id(Arc), Dict),
			 SetNodesOri, SetNodesToRenameOri, NodeDest};
	{true, false} -> {Dict, SetNodesOri, SetNodesToRenameOri, NodeDest};
	_ -> {dict:store(Id, Arc, Dict), SetNodesOri,
	      SetNodesToRenameOri, NodeDest}
    end;
remove_or_tag_arcs_between_fold(Id, Arc, {Dict, SetNodesOri,
					  SetNodesToRenameOri, NodeDest}) ->
    {dict:store(Id, Arc, Dict), SetNodesOri, SetNodesToRenameOri, NodeDest}.

remove_orphan_arcs(#drai{dnodes = DNodes,
			darcs = DArcs} = Drai) ->
    Drai#drai{darcs = element(1, dict:fold(fun remove_orphan_arcs_fold/3, {dict:new(), DNodes}, DArcs))}.
remove_orphan_arcs_fold(Id, #diagram_arc{id_start = IdTo, id_end = IdFrom} = Arc, {Dict, DNodes}) ->
    {case (dict:is_key(IdTo, DNodes) andalso dict:is_key(IdFrom, DNodes)) of
	 true -> dict:store(Id, Arc, Dict);
	 false -> Dict
     end, DNodes}.

get_node_lists_to_diamond(#drai{dnodes = DNodes} = Drai) ->
    element(
      1, dict:fold(
	   fun get_node_lists_to_diamond_fold/3,
	   {[], Drai}, DNodes)).

get_node_lists_to_diamond_fold(Key, Value, {Acc, Drai}) ->
    {case get_nod_id(Value) of
	 ("diamond" ++ _) = DiaId ->
	     NodeIds = expand_nodes_up(sets:from_list([Key]), Drai),
	     [{DiaId, lists:map(fun (Id) -> dict:fetch(Id, Drai#drai.dnodes) end,
				sets:to_list(NodeIds))}|Acc];
	 _ -> Acc
     end, Drai}.

% Find reverse dependencies for a list of nodes (control nodes)
-spec expand_nodes_within_cluster(#drai{}, [#diagram_node{}]) -> {sets:set(string()), dict:dict(string(), sets:set(string()))}.
expand_nodes_within_cluster(Drai, NodeList) ->
	% We get list of lists of nodes grouped by cluster
	ClusterGroups = utils:group_by(fun get_cluster_id/1, NodeList),
	% For each node in each group we do depth-first search
  merge_results([expand_cluster(Drai, ClusterGroup) || ClusterGroup <- ClusterGroups]).

% We do depth-first search to find reverse dependencies within each cluster
% We keep an alternative dict with parent -> child dict (Deps) and a set of covered nodes (Deps)
-spec expand_cluster(#drai{}, [#diagram_node{}]) -> {sets:set(string()), dict:dict(string(), sets:set(string()))}.
expand_cluster(Drai, NodeList) ->
  expand_cluster_aux(Drai, NodeList, sets:from_list(get_nod_ids(NodeList)), dict:new()).
expand_cluster_aux(_Drai, [], Nodes, Deps) -> {Nodes, Deps};
expand_cluster_aux(Drai, [#diagram_node{id = Id} = Node|Rest], Nodes, Deps) ->
	ValidChildren = expand_cluster_expand_one_aux(Drai, Id, Nodes, get_cluster_id(Node)),
	NewDeps = add_to_deps(Id, ValidChildren, Deps),
	NewNodes = sets:union(Nodes, ValidChildren),
	expand_cluster_aux(Drai, resolve_ids(Drai, sets:to_list(ValidChildren)) ++ Rest, NewNodes, NewDeps).

% We merge the list of tuples into a single tuple
% Tuples have { (set of covered nodes), (dict of parent -> child relations dict(id, set(ids)))
-spec merge_results([{sets:set(string()), dict:dict(string(), sets:set(string()))}]) -> {sets:set(string()), dict:dict(string(), sets:set(string()))}.
merge_results(List) -> lists:foldl(fun merge_results_aux/2, {sets:new(), dict:new()}, List).
merge_results_aux({Set1, Dict1}, {Set2, Dict2}) ->
	{sets:union(Set1, Set2), dict:merge(fun (_, A, B) -> sets:union(A, B) end, Dict1, Dict2)}.

% We find children nodes that are not in the set and are in the same cluster as the parent
-spec expand_cluster_expand_one_aux(#drai{}, string(), sets:set(string()), any()) -> sets:set(string()).
expand_cluster_expand_one_aux(Drai, Id, Nodes, Cluster) ->
	RawChildrenSet = dia_utils:expand_diamonds_down(Drai, dia_utils:expand_nodes_down(sets:from_list([Id]), Drai)),
	ChildrenSet = sets:subtract(RawChildrenSet, Nodes),
	sets:filter(fun_in_cluster(Drai, Cluster), ChildrenSet).

% Produces a function that takes a node id and returns true iif the node is in the cluster Cluster
-spec fun_in_cluster(#drai{}, any()) -> fun((string()) -> (boolean())).
fun_in_cluster(Drai, Cluster) ->
	fun (Id) -> case dict:find(Id, Drai#drai.dnodes) of
								{ok, Node} -> get_cluster_id(Node) =:= Cluster;
		            _ -> false
							end
	end.

% Adds the set of ValidChildren to the dict of (parent -> set(children)), where both parent and children are nodeIds
-spec add_to_deps(string(), sets:set(string()), dict:dict(string(), sets:set(string()))) -> dict:dict(string(), sets:set(string())).
add_to_deps(Id, ValidChildren, Deps) ->
	dict:update(Id, fun_add_to_set(ValidChildren), ValidChildren, Deps).
fun_add_to_set(ValidChildren) ->
	fun (Set) -> sets:union(Set, ValidChildren) end.

% Resolves a list of node ids to a list of node records
-spec resolve_ids(#drai{}, [string()]) -> #diagram_node{}.
resolve_ids(Drai, NodeIdList) ->
  lists:map(fun_resolve_id(Drai), NodeIdList).
fun_resolve_id(Drai) ->
	fun (Id) -> dict:fetch(Id, Drai#drai.dnodes) end.


%% Loop detection
%% ==============

highlight_loops(Drai) ->
    TopNodesSet = sets:from_list(dict:fetch_keys(Drai#drai.dnodes)),
    ArcSet = element(1, sets:fold(fun dfs_loop_detection/2,
				  {sets:new(), sets:new(), Drai},
				  TopNodesSet)),
    highlight_arcs_set(ArcSet, Drai).

dfs_loop_detection(Node, {ArcSet, Visited, Drai}) ->
    ThisNodeSet = sets:from_list([Node]),
    NewVisited = sets:add_element(Node, Visited),
    ExpandedNodeSet = expand_nodes_down_wt_arcfilter(fun is_data_dep/1, ThisNodeSet, Drai),
    NewArcSet = element(1, sets:fold(fun get_loopback_arcs_fold/2,
				     {ArcSet, NewVisited},
				     sets:filter(fun is_data_dep/1, get_arcs_down(ThisNodeSet, Drai)))),
    NewNodeSet = sets:subtract(ExpandedNodeSet, NewVisited),
    {element(1, sets:fold(fun dfs_loop_detection/2,
			  {NewArcSet, NewVisited, Drai},
			  NewNodeSet)),
     Visited, Drai}.

get_top_nodes(#drai{dnodes = DNodes,
		    arcst = ArcsTo}) ->
    EndNodes = dict:fold(fun(Key, _, Set) -> sets:add_element(Key, Set) end,
			 sets:new(), ArcsTo),
    sets:subtract(sets:from_list(dict:fetch_keys(DNodes)), EndNodes).

get_control_nodes(#drai{dnodes = DNodes}) ->
    dict:fold(fun add_control_nodes_to_set/3,
	      sets:new(), DNodes).

add_control_nodes_to_set(Key, Node, Set) ->
    case is_control_node(Node) of
	false -> Set;
	true -> sets:add_element(Key, Set)
    end.

get_loopback_arcs_fold(Value, {Set, VisitedNodeSet}) ->
    case sets:is_element(get_arc_to(Value), VisitedNodeSet) of
	true -> {sets:add_element(Value, Set), VisitedNodeSet};
	false -> {Set, VisitedNodeSet}
    end.

highlight_arcs_set(ArcSet, Drai) ->
    ThickArcList = lists:map(fun set_arc_thick_and_mark_loop/1, sets:to_list(ArcSet)),
    update_arcs(ThickArcList, Drai).

set_arc_thick_and_mark_loop(#diagram_arc{properties = List} = Arc) ->
    Arc#diagram_arc{properties = [thick|List], is_loop = true}.

generate_subgraphs(#drai{dnodes = DNodes} = Drai) ->
    {_ClusterD, BaseClusterNodesSet, DNodes2} = dict:fold(fun get_base_cluster_nodes/3, {dict:new(), sets:new(), DNodes}, DNodes),
    {NewDrai,_,_} = sets:fold(fun depth_search_inc_in_clus/2, {Drai#drai{dnodes = DNodes2}, [], no}, BaseClusterNodesSet),
    sets:fold(fun find_class/2, NewDrai, BaseClusterNodesSet).

-spec find_class(#diagram_node{}, #drai{}) -> #drai{}.
find_class(#diagram_node{id = NodeId, cluster = Cluster, tags = Tags} = Node, #drai{dnodes = DNodes} = Drai) ->
	ChildNodes = dia_utils:expand_nodes_down(sets:from_list([NodeId]), Drai),
	NamesAndNodes = get_method_names([], Cluster, ChildNodes, Drai),
	{Class, NewDNodes} = compute_class_and_set_for_children(NamesAndNodes, Tags, DNodes),
	Drai#drai{dnodes = dict:store(NodeId, Node#diagram_node{class = Class}, NewDNodes)}.

get_method_names(MNList, Cluster, NodeSet, Drai) ->
	case sets:size(NodeSet) of
		0 -> MNList;
		_ -> {NodeIdSet, ListOfMethodNames, _} =
			 lists:foldl(fun get_method_names_aux/2, {sets:new(), MNList, Cluster},
				     get_nodes_by_ids(sets:to_list(NodeSet), Drai)),
		     get_method_names(ListOfMethodNames, Cluster, expand_nodes_down(NodeIdSet, Drai), Drai)
	end.

get_method_names_aux(#diagram_node{id = Id, cluster = Cluster, http_request = no,
				   content = #callback{method_name = MethodName}} = Node,
					 {NodeIdSet, MethodNameList, Cluster}) ->
	{sets:add_element(Id, NodeIdSet), [{MethodName, Node}|MethodNameList], Cluster};
get_method_names_aux(_, {_,_,_} = Acc) -> Acc.

compute_class_and_set_for_children(List, Tags, DNodes) ->
    {ContainsErrorSubstring, NewDNodes} = lists:foldl(fun check_class/2, {false, DNodes}, List),
    {case {ContainsErrorSubstring, lists:member(is_after, Tags)} of
	 {_, true} -> tearDown;
	 {true, _} -> error;
	 {false, _} -> normal
     end, NewDNodes}.

check_class({MethodName, #diagram_node{id = Id} = Node},
	    {ContainsErrorSubstring, DNodes}) ->
    case contains_error_substring(MethodName) of
	true -> {true, dict:store(Id, Node#diagram_node{class = error}, DNodes)};
	false -> {ContainsErrorSubstring, DNodes}
    end.

contains_error_substring(String) ->
	contains_error_substring_aux(string:to_lower(String)).
contains_error_substring_aux([]) -> false;
contains_error_substring_aux("errors" ++ _) -> false;
contains_error_substring_aux("error" ++ _) -> true;
contains_error_substring_aux("fail" ++ _) -> true;
contains_error_substring_aux([_|Rest]) -> contains_error_substring_aux(Rest).

depth_search_inc_in_clus(#diagram_node{cluster = ClusterN}, {Drai, Trace, Cluster})
  when (ClusterN =/= no) andalso (Cluster =/= no) ->
    {Drai, Trace, Cluster};
depth_search_inc_in_clus(#diagram_node{id = NodeId, cluster = ClusterN} = Node,
			 {Drai, Trace, ClusterAc}) ->
    NewTrace = [Node|Trace],
	Cluster = case ClusterAc of
				  no -> ClusterN;
				  Else -> Else
			  end,
    NodeDownIdSet = dia_utils:expand_nodes_down(sets:from_list([NodeId]), Drai),
    NodeDownSet = sets:from_list(get_nodes_by_ids(sets:to_list(NodeDownIdSet), Drai)),
	case sets:size(NodeDownIdSet) of
		0 -> {add_to_cluster(NewTrace, Cluster, Drai), Trace, ClusterAc};
		_ -> {NewDrai, _, _} = sets:fold(fun depth_search_inc_in_clus/2, {Drai, NewTrace, Cluster}, NodeDownSet),
			 {NewDrai, Trace, ClusterAc}
	end.

add_to_cluster([], _, Drai) -> Drai;
add_to_cluster([#diagram_node{id = NodeId} = Node|Rest], Cluster, #drai{dnodes = DNodes} = Drai) ->
    NewDrai = Drai#drai{dnodes = dict:store(NodeId, Node#diagram_node{cluster = Cluster}, DNodes)},
    add_to_cluster(Rest, Cluster, NewDrai).

get_base_cluster_nodes(NodeId, #diagram_node{http_request = {Method, URL} = HR} = Node, {ClusterDict, Set, DNodes}) ->
    ClusterId = case dict:find(HR, ClusterDict) of
		    {ok, CI} -> CI;
		    error -> NodeId
		end,
    NewNode = Node#diagram_node{cluster = {cluster, ClusterId, parser_utils:print_escaped({Method, URL})}},
    {dict:store(HR, ClusterId, ClusterDict), sets:add_element(NewNode, Set), dict:store(NodeId, NewNode, DNodes)};
get_base_cluster_nodes(_, _, Acc) -> Acc.


is_control_node(#diagram_node{http_request = no}) -> false;
is_control_node(_) -> true.


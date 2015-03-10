%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela Seijas
%%% @doc
%%% Calculates min depth of paths in a directed graph
%%% @end
%%% Created :  9 Mar 2015 by Pablo Lamela Seijas
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

-module(depth_calculator).

-export([get_edge_depths/2]).

-record(alg_data, {node_dict, arc_indx, curr_nodes, edge_up, edge_down}).
-record(node_info, {type, weight, is_final}).

get_edge_depths(Nodes, Edges) ->
    {NodeSet, NodeDict} = lists:foldl(fun add_node/2, {sets:new(), dict:new()}, Nodes),
    {TopNodes, ArcIndx, EdgeUp, EdgeDown} =
	lists:foldl(fun add_arc/2, {NodeSet, dict:new(), dict:new(), dict:new()}, Edges),
    AlgDataEnd = algorithm(#alg_data{node_dict = NodeDict,
				     arc_indx = ArcIndx,
				     curr_nodes = lqueue:init(sets:to_list(TopNodes)),
				     edge_up = EdgeUp, edge_down = EdgeDown}),
    alg_pos(AlgDataEnd).

algorithm(#alg_data{
	     node_dict = NodeDict,
	     edge_up = EdgeUp,
	     edge_down = EdgeDown,
	     curr_nodes = CurrNodes
	    } = Data) ->
    case lqueue:pop(CurrNodes) of
	loop -> Data;
	{ok, NodeId, CurrNodes2} ->
	    begin
		({_OldWeight, _OldFinal} = OldWeightComplete) = get_weight(NodeId, NodeDict),
		({NewWeight, NewFinal} = NewWeightComplete) = compute_weight(NodeId, NodeDict, EdgeUp),
		{CurrNodes6, NodeDict3} =
		    case OldWeightComplete =:= NewWeightComplete of
			false ->
			    begin
				CurrNodes3 = case NewFinal of
						 true -> CurrNodes2;
						 false -> lqueue:cons(NodeId, CurrNodes2)
					     end,
				CurrNodes4 = case (NewWeight =/= inf) of
						 true -> expand_children(NodeId, EdgeDown, CurrNodes3);
						 false -> CurrNodes3
					     end,
				CurrNodes5 = lqueue:reset_loop(CurrNodes4),
				NodeDict2 = update_weight(NodeId, NewWeight, NewFinal, NodeDict),
				{CurrNodes5, NodeDict2}
			    end;
			true -> {CurrNodes2, NodeDict}
		    end,
		algorithm(Data#alg_data{node_dict = NodeDict3,
					curr_nodes = CurrNodes6})
	    end
    end.

get_weight(NodeId, NodeDict) ->
    #node_info{weight = Weight,
	       is_final = Final} = dict:fetch(NodeId, NodeDict),
    {Weight, Final}.

update_weight(NodeId, NewWeight, NewFinal, NodeDict) ->
    NodeBase = dict:fetch(NodeId, NodeDict),
    dict:store(NodeId, NodeBase#node_info{
			 weight = NewWeight,
			 is_final = NewFinal
			}, NodeDict).

expand_children(NodeId, EdgeDown, CurrNodes) ->
    sets:fold(fun lqueue:cons/2, CurrNodes, get_nodes_throw_def(NodeId, EdgeDown)).

compute_weight(NodeId, NodeDict, EdgeUp) ->
    (#node_info{type = Type}) = dict:fetch(NodeId, NodeDict),
    Weights = sets:fold(fun (X, Y) -> [get_weight(X, NodeDict)|Y] end,
			[], get_nodes_throw_def(NodeId, EdgeUp)),
    case lists:unzip(Weights) of
	{[], []} -> {0, true};
	{[H|T], Final} -> {tolerant_add_one(lists:foldl(case Type of
							    sum -> fun sum_weights/2;
							    min -> fun min_weights/2
							end, H, T)),
			   lists:all(fun (X) -> X end, Final)}
    end.

tolerant_add_one(inf) -> inf;
tolerant_add_one(N) -> N + 1.

sum_weights(W1, W2)
  when (W1 =:= inf) orelse (W2 =:= inf) -> inf;
sum_weights(Weight1, Weight2) -> Weight1 + Weight2.

min_weights(inf, Weight2) -> Weight2;
min_weights(Weight1, inf) -> Weight1;
min_weights(Weight1, Weight2) when Weight1 < Weight2 -> Weight1;
min_weights(_Weight1, Weight2) -> Weight2.

get_nodes_throw_def(NodeId, EdgeDown) ->
    case dict:find(NodeId, EdgeDown) of
	{ok, Value} -> Value;
	error -> sets:new()
    end.

alg_pos(#alg_data{
	   node_dict = NodeDict,
	   arc_indx = ArcIndx
	  }) ->
    {List, _} = dict:fold(fun get_edge_and_weight/3, {[], NodeDict}, ArcIndx),
    List.

add_arc({From, ArcId, To}, {TopNodes, ArcIndx, EdgeUp, EdgeDown}) ->
    {sets:del_element(To, TopNodes),
     dict:store(ArcId, From, ArcIndx),
     store_set_dict(To, From, EdgeUp),
     store_set_dict(From, To, EdgeDown)}.

store_set_dict(Key, Val, Dict) ->
    dict:update(Key, fun (X) -> sets:add_element(Val, X) end, sets:from_list([Val]), Dict).

add_node({NodeId, Type}, {NodeSet, NodeDict}) ->
    {sets:add_element(NodeId, NodeSet),
     dict:store(NodeId,
		#node_info{type = Type,
			   weight = inf,
			   is_final = false},
		NodeDict)}.

get_edge_and_weight(ArcId, NodeId, {List, NodeDict}) ->
    #node_info{weight = Weight} = dict:fetch(NodeId, NodeDict),
    {[{ArcId, Weight}|List], NodeDict}.

%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Procedure to collapse string bubbles
%%% @end
%%% Created : 18 Jul 2014 by Pablo Lamela Seijas
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
-module(string_combinator).

-include("records.hrl").

-export([combinate_strings/1]).

combinate_strings(Dia) ->
    foreach_node(Dia, fun try_to_combine/2).

% Loop through the list of nodes
foreach_node(#drai{dnodes = DNodes} = Dia, Fun) ->
    lists:foldl(fun (X, Y) -> combinator(Fun, X, Y) end, Dia,
		dict:fetch_keys(DNodes)).

combinator(Fun, NodeId, Dia) ->
    case Fun(NodeId, Dia) of
	{true, NewDia} -> combinator(Fun, NodeId, NewDia);
	false -> Dia
    end.

% Combine NodeId downwards until impossible
try_to_combine(NodeOneId, Dia) ->
    try 
	begin
	    NodeOne = get_node(NodeOneId, Dia),
	    [NodeTwoId] = check_one_list(expand_down(NodeOneId, Dia)),
	    NodeTwo = get_node(NodeTwoId, Dia),
	    {Str1Id, P1Arc, O1Arc} = check_struct(get_arcs_up(NodeOneId, Dia)),
	    Str1Node = get_node(Str1Id, Dia),
	    {Str2Id, P2Arc, O2Arc} = check_struct(get_arcs_up(NodeTwoId, Dia)),
	    Str2Node = get_node(Str2Id, Dia),
	    MergedString = merge_strings(Str1Node, Str2Node),
	    MergedNode = merge_append_nodes(NodeOne, NodeTwo),
	    Dia2 = set_node(MergedString, Dia),
	    Dia3 = set_node(MergedNode, Dia2),
	    Dia4 = set_arc(merge_tags(P1Arc, P2Arc), Dia3),
	    Dia5 = set_arc(merge_tags(O1Arc, O2Arc), Dia4),
	    Dia6 = move_returns(NodeTwoId, NodeOneId, Dia5),
	    Dia7 = remove_node(NodeTwoId, Dia6),
	    remove_node(Str2Id, Dia7)
	end
    of R -> {true, R}
    catch
    	throw:not_compliant -> false
    end.

% Check Funs
% ==========
check_one_list([_] = L) -> L;
check_one_list(_) -> throw(not_compliant).

check_struct(ArcList) -> check_struct_check(lists:sort(lists:map(fun get_this_parent_id/1, ArcList))).
check_struct_check([{false, OArc}, {true, Id, PArc}]) -> {Id, PArc, OArc};
check_struct_check(_) -> throw(not_compliant).

get_this_parent_id(#diagram_arc{id_start = Id, content = {param,1}} = Arc) -> {true, Id, Arc};
get_this_parent_id(#diagram_arc{} = Arc) -> {false, Arc}.

merge_strings(#diagram_node{
		 label = {string, String},
		 is_label_term = true,
		 content = 
		     #value{type = string,value = String} = V} = D,
	      #diagram_node{
		 label = {string, String2},
		 is_label_term = true,
		 content = 
		     #value{type = string,value = String2}}) ->
    CombinedString = String ++ String2,
    D#diagram_node{
      label = {string, CombinedString},
      content = V#value{value = CombinedString}};
merge_strings(_, _) -> throw(not_compliant).

merge_append_nodes(#diagram_node{
		      label = "append",
		      is_label_term = false,
		      content = 
			  #callback{
			     kind = exit_method,depth = Depth,is_dynamic = true,
			     method_name = "append",
			     class_signature = "Ljava/lang/StringBuilder;",
			     params = 
				 [#value{type = string,value = String1} = V1],
			     return = 
				 #value{
				    type = object,
				    obj_info = 
					#obj_info{first_time = false,identifier = SBId}
				   },
			     this = 
				 #value{
				    type = object,value = [],
				    obj_info = 
					#obj_info{first_time = false,identifier = SBId}
				   }
			    } = Cb,
		      tags = Tags1} = N1,
		   #diagram_node{
		      id = _,label = "append",
		      is_label_term = false,
		      content = 
			  #callback{
			     kind = exit_method,depth = Depth,is_dynamic = true,
			     method_name = "append",
			     class_signature = "Ljava/lang/StringBuilder;",
			     params = 
				 [#value{type = string, value = String2}],
			     return = 
				 #value{
				    type = object,
				    obj_info = 
					#obj_info{first_time = false,identifier = SBId}},
			     this = 
				 #value{
				    type = object,value = [],
				    obj_info = 
					#obj_info{first_time = false,identifier = SBId}
				   }
			    },
		      tags = Tags2
		     }) ->
    N1#diagram_node{
      content = 
	  Cb#callback{
	    method_name = "append",
	    class_signature = "Ljava/lang/StringBuilder;",
	    params = [V1#value{value = String1 ++ String2}]},
      tags = lists:usort(Tags1 ++ Tags2)};
merge_append_nodes(_, _) -> throw(not_compliant).

merge_tags(#diagram_arc{tags = Tags1} = Arc, #diagram_arc{tags = Tags2}) ->
    Arc#diagram_arc{tags = Tags1 ++ Tags2}.

% External calls
% ==============

get_node(NodeId, Dia) ->
    case dia_utils:get_node_by_id(NodeId, Dia) of
	{ok, Node} -> Node;
	error -> throw(not_compliant)
    end.

expand_down(NodeId, Dia) ->
    sets:to_list(dia_utils:expand_nodes_down(sets:from_list([NodeId]), Dia)).

get_arcs_up(NodeId, Dia) ->
    sets:to_list(dia_utils:get_arcs_up(sets:from_list([NodeId]), Dia)).

set_node(Node, Dia) -> dia_utils:set_node(Node, Dia).

set_arc(Arc, Dia) -> dia_utils:set_arc(Arc, Dia).

move_returns(NodeOriId, NodeDestId, Dia) -> dia_utils:move_returns(NodeOriId, NodeDestId, Dia).

remove_node(NodeId, Dia) -> dia_utils:remove_node(NodeId, Dia).

%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2015, Pablo Lamela Seijas
%%% @doc
%%% Test Suite for depth calculator
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

-module(depth_calculator_eqc).

-include_lib("eqc/include/eqc.hrl").

-compile(export_all).

% Node = {node_id, type}
% Edge = {ini_node, edge_id, end_node}
% Type = min | plus

gen_graph() -> ?SIZED(Size, gen_graph(Size)).

gen_graph(Size) ->
    NodeIds = ["node" ++ integer_to_list(Id) || Id <- lists:seq(1, Size)],
    case Size of
	0 -> {[], []};
	_ -> ?LET(Links, vector((Size * 3) div 2, {elements(NodeIds), elements(NodeIds)}),
		  begin
		      UniqueLinks = lists:usort(Links),
		      EdgeIds = ["arc" ++ integer_to_list(Id) || Id <- lists:seq(1, length(UniqueLinks))],
		      {?LET(List, [{NodeId, elements([min, sum])} || NodeId <- NodeIds], shuffle(List)),
		       shuffle([{A, C, B} || {{A, B}, C} <- lists:zip(UniqueLinks, EdgeIds)])}
		  end)
    end.

% Generic prop
generic_prop(Fun) ->
    ?FORALL({Nodes, Edges},gen_graph(),
	    ?TIMEOUT(1000,
		     begin
			 Depths = depth_calculator:get_edge_depths(Nodes, Edges),
			 case Edges of
			     [] -> Depths =:= [];
			     _ -> ?FORALL({EdgeId, Depth}, elements(Depths),
					  begin
					      Fun(Depth, EdgeId, {Nodes, Edges}, Depths)
					  end)
			 end
		     end)).


% Prop is real

prop_is_real() -> generic_prop(fun (A, B, C, _) -> is_at_least(A, B, C) end).

is_at_least(inf, EdgeId, Graph) ->
    is_at_least(100, EdgeId, Graph);
is_at_least(0, _EdgeId, _Graph) -> true;
is_at_least(N, EdgeId, Graph) ->
    {ok, {Node, Type}} = get_node_up(EdgeId, Graph),
    is_at_least_nod(N, Node, Type, Graph).

is_at_least_nod(0, _NodeId, _, _Graph) -> true;
is_at_least_nod(N, NodeId, min, Graph) ->
    case get_arcs_up(NodeId, Graph) of
	[] -> N;
	UpArcs -> ?LET(X, elements(UpArcs),
		       is_at_least(tolerant_dec(N), X, Graph))
    end;
is_at_least_nod(N, NodeId, sum, Graph) ->
    case get_arcs_up(NodeId, Graph) of
	[] -> N;
	UpArcs -> lists:foldl(
		    fun (El, Acc) -> 
			    ?LET(GenAcc, Acc,
				 case GenAcc of
				     L when is_atom(L) -> L;
				     _ -> ?LET(RecursiveCall, is_at_least(GenAcc, El, Graph),
					       case RecursiveCall of
						   M when is_integer(M) -> M;
						   Result -> Result
					       end)
				 end)
		    end, tolerant_dec(N), UpArcs)
    end.

% Prop is min

prop_is_min() -> generic_prop(fun follow_min/4).

follow_min(N, EdgeId, Graph, Depths) ->
    {ok, {NodeId, Type}} = get_node_up(EdgeId, Graph),
    follow_min_nod(N, NodeId, Type, Graph, Depths).

follow_min_nod(inf, NodeId, Type, Graph, Depths) ->
    follow_min_nod(100, NodeId, Type, Graph, Depths);
follow_min_nod(0, _NodeId, _Type, _Graph, _Depths) -> true;
follow_min_nod(N, NodeId, sum, Graph, Depths) ->
    Arcs = ordered_arcs(get_arcs_up(NodeId, Graph), Depths),
    case tolerant_sum([1|element(2, lists:unzip(Arcs))]) of
	M when (M =:= inf) orelse (M >= N) ->
	    ?LET({ArcId, NewN}, elements(Arcs),
		 follow_min(min(NewN, tolerant_dec(N)), ArcId, Graph, Depths)
		);
	_ -> false
    end;
follow_min_nod(N, NodeId, min, Graph, Depths) ->
    List = get_arcs_up(NodeId, Graph),
    NextN = tolerant_dec(N),
    case ordered_arcs(List, Depths) of
	[{ArcId, inf}|_] -> follow_min(NextN, ArcId, Graph, Depths);
	[{ArcId, M}|_] when M >= NextN -> follow_min(NextN, ArcId, Graph, Depths);
	_ -> false
    end.

tolerant_sum(List) -> tolerant_sum_aux(List, 0).
tolerant_sum_aux([inf|_], _) -> inf;
tolerant_sum_aux([A|Rest], B) -> tolerant_sum_aux(Rest, A + B);
tolerant_sum_aux([], B) -> B.

tolerant_dec(N) when is_integer(N) -> N - 1;
tolerant_dec(Else) -> Else.

% Utils

get_node_up(EdgeId, {Nodes, Edges}) ->
    case lists:keyfind(EdgeId, 2, Edges) of
	false -> throw({no_edge, EdgeId});
	{NodeUp, EdgeId, _} ->
	    case lists:keyfind(NodeUp, 1, Nodes) of
		false -> error;
		{NodeUp, _} = Res -> {ok, Res}
	    end
    end.

get_arcs_up(NodeId, {_, Edges}) ->
    [EdgeId || {_, EdgeId, ND} <- Edges, ND =:= NodeId].

ordered_arcs(List, Depths) ->
    lists:keysort(2, [{ArcId, Depth} || {ArcId, Depth} <- Depths, ArcId2 <- List, ArcId =:= ArcId2]).
    

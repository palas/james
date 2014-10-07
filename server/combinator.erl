%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Procedures to collapse diagrams
%%% @end
%%% Created : 26 Jun 2014 by Pablo Lamela Seijas
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
-module(combinator).

-include("records.hrl").

-export([combinate/3]).

combinate(SN,SA,Config) ->
    Island = dia_utils:create_drai(SN, SA),
    CombinatedIsland = clean_and_combinate(Island, Config),
    {Nodes, Arcs} = islands:fusion_islands([CombinatedIsland]),
    OIslands = islands:get_islands(Nodes, Arcs),
    io:format("~p Islands!~n", [length(OIslands)]),
    case Config#config.single_file of
	false -> [islands:fusion_islands([OneIsland])
		  || OneIsland <- islands:get_biggest_islands(Config#config.num_of_islands, OIslands)];
	true -> [islands:fusion_islands(
		   islands:get_biggest_islands(Config#config.num_of_islands, OIslands))]
    end.

%% Diagram level functions
%% =======================

clean_and_combinate(Island, Config) ->
    combinate_aux(dia_utils:remove_up_from(Config#config.remove_nodes_up_from, Island), Config).

combinate_aux(Island,Config) ->
    op(fun dia_utils:remove_elliptic_nodes/1, Config#config.remove_bubbles,
       op(fun dia_utils:highlight_loops/1, Config#config.highlight_loops,
	  op(fun string_combinator:combinate_strings/1, Config#config.collapse_strings,
	     dia_utils:rebuild_idxs(
	       op(fun dia_utils:collapse_integers/1, Config#config.collapse_integers,
		  dia_utils:rebuild_idxs(
		    dia_utils:generate_diamonds_in_drai(
		      op(fun dia_utils:remove_orphan_nodes/1, Config#config.remove_orphan_nodes,
			 dia_utils:remove_duplicated_arcs(
			   combinate_aux2(
			     dia_utils:generate_subgraphs(Island),
			     Config,0,Config#config.max_iterations)))))))))).

op(Fun, true, Input) -> Fun(Input);
op(_, false, Input) -> Input.

combinate_aux2(Island,_,MaxIterations,MaxIterations) -> Island;
combinate_aux2(Island,Config,Iteration,MaxIterations) ->
    gest_best_path_pair_and_do(Island, Config, Iteration, MaxIterations, fun combinate_aux3/5).

combinate_aux3(Island, Config, Iteration, _, none) -> catch_count_iterations(Island, Iteration, Config);
combinate_aux3(Island, Config, Iteration, MaxIterations, {Path1, Path2}) ->
    combinate_aux2(path_utils:join_path_pair(Path1, Path2, Island),Config,
                   Iteration + 1,MaxIterations).

gest_best_path_pair_and_do(Island, Config, Iteration, MaxIterations, F) ->
    case get_best_path_pair(Config#config.big_k_value, Island, false) of
	none -> case get_best_path_pair(Config#config.small_k_value, Island, true) of
		    none -> F(Island, Config, Iteration, MaxIterations, none);
		    {Path1, Path2} -> F(Island, Config, Iteration,
					MaxIterations, {Path1, Path2})
		end;
	{Path1, Path2} -> F(Island, Config, Iteration,
			    MaxIterations, {Path1, Path2})
    end.


catch_count_iterations(_Island, Iteration, #config{max_iterations = count_iterations}) ->
	throw({count_iterations, Iteration});
catch_count_iterations(Island, _, _) -> Island.

get_best_path_pair(N,Island,OnlyCommon) ->
    NormalNodeList = dia_utils:get_normal_nodes(Island),
    PathList = initialise_paths(NormalNodeList, Island),
    A = get_best_path_from_group(PathList,Island,1,OnlyCommon),
    case A of
	none -> none;
	{Path1, Path2, S} ->
	    case {path_utils:path_length(Path1), path_utils:path_length(Path2), S} of
		{L, L, _} when L >= N ->
		    io:format("PathLengths: ~p ~p ~p~n", [path_utils:path_length(Path1),
							  Path1#path.direction,
							  OnlyCommon]),
		    {Path1, Path2};
		_ -> none
	    end
    end.

get_best_path_from_group(GroupList,Island,Level,OnlyCommon) ->
    ListOfLists = path_utils:group_paths(GroupList),
    FilteredListOfLists = expanding_filter(ListOfLists),
    case FilteredListOfLists of
	[[_, _|_]|_] -> % This level may not be useful, but next could
	    NewBests = best_paths_from_groups(FilteredListOfLists,Island,Level + 1,OnlyCommon), % Compute next level
	    case NewBests of
		[] -> % lets check if this level has something
		    case additional_last_level_filter(FilteredListOfLists, Island, OnlyCommon) of
			[[P1, P2|_] = SubList|_] -> % This level is useful and the best this path
			    {P1, P2, length(SubList)};
			_ -> none
		    end; % Next level did not cut it
		[{PI1, PI2, L}|_] ->
		    {PI1, PI2, L} % Next level did cut it
	    end;
	_ -> none
    end.

additional_last_level_filter(ListOfLists, _Island, false) -> ListOfLists;
additional_last_level_filter(ListOfLists, Island, true) ->
    only_common_and_border(ListOfLists, Island).

expanding_filter(ListOfLists) ->
    lists:filter(fun (X) -> length(X) > 1 end, ListOfLists).

collapsing_filter(NewBests) ->
    lists:filter(fun (none) -> false; (_) -> true end, NewBests).

sort_by_path_depth(List) ->
    lists:reverse(utils:sort_using(fun ({Path1, _, T}) -> {path_utils:get_path_depth(Path1), T} end, List)).

best_paths_from_groups(ListOfLists,Island,Level,OnlyCommon) ->
    sort_by_path_depth(
      collapsing_filter(
	lists:map(fun (List) ->
			  AdvList = path_utils:advance_paths(List, Island),
			  get_best_path_from_group(AdvList,Island,Level,OnlyCommon)
		  end, ListOfLists))).


initialise_paths(NodeList, Islands) ->
    lists:concat(lists:map(fun (X) -> [path_utils:initialise_path(X, Islands, downwards),
				       path_utils:initialise_path(X, Islands, upwards)] end,
			   NodeList)).

remove_empties([]) -> [];
remove_empties([[]|Rest]) -> remove_empties(Rest);
remove_empties([[_]|Rest]) -> remove_empties(Rest);
remove_empties([El|Rest]) -> [El|remove_empties(Rest)].

only_common_and_border(List, Drai) -> remove_empties(
			     lists:map(fun (X) -> only_border_aux(X, [], Drai) end, List)
			     ++ lists:map(fun (X) -> only_common_aux(X, sets:new(), dict:new(), Drai) end, List)).

only_border_aux([], List, _) -> List;
only_border_aux([#path{node_ids = [LastNodes|_],
		       direction = Dir} = Path|Rest], List, Drai) ->
    OriNodeSet = sets:from_list(LastNodes),
    Expansion = case Dir of
		    upwards -> dia_utils:expand_nodes_up(OriNodeSet, Drai);
		    downwards -> dia_utils:expand_nodes_down(OriNodeSet, Drai)
		end,
    case sets:size(sets:subtract(Expansion, OriNodeSet)) of
	0 -> only_border_aux(Rest, [Path|List], Drai);
	_ -> only_border_aux(Rest, List, Drai)
    end.

only_common_aux([], _, _, _) -> [];
only_common_aux([(#path{node_ids = NodeIds,
		        ori_node = OriNode} = Path)|Rest], Set, Dict, Drai) ->
    DeltaSet = sets:union(sets:from_list(lists:concat(NodeIds)),
			  dia_utils:expand_nodes_down(sets:from_list([dia_utils:get_nod_id(OriNode)]),
						      Drai)),
    Inter = sets:intersection(Set, DeltaSet),
    case sets:size(Inter) of
	0 ->
	    begin
		NewDict = sets:fold(fun (X, D) -> dict:store(X, Path, D) end, Dict, DeltaSet),
		NewSet = sets:union(Set, DeltaSet),
		only_common_aux(Rest, NewSet, NewDict, Drai)
	    end;
	_ ->
	    begin
		[El|_] = sets:to_list(Inter),
		[dict:fetch(El, Dict), Path]
	    end
    end.

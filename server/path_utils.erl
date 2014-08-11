%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Procedures that work with paths
%%% @end
%%% Created : 30 Jun 2014 by Pablo Lamela Seijas
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
-module(path_utils).

-include("records.hrl").

-export([get_path_depth/1, advance_paths/2, join_path_pair/3, path_length/1,
	 group_paths/1, initialise_path/3]).

get_path_depth(Path) -> Path#path.depth.

% Takes a List of paths, and pushes it
% in the direction opposite to the arrows.
% In case of several possibilities,
% those will be stored inside each Path
advance_paths(List, Island) ->
    lists:map(fun (X) -> advance_path(X, Island) end, List).
advance_path(#path{node_ids = [List|Rest],
		   depth = N,
		   direction = Dir} = Path, Island) ->
    TempNewNodeTuples = lists:concat([[{dia_utils:get_tran_prop(Tran),
					case Dir of
					    upwards -> dia_utils:expand_tran_upwards(Tran, Island);
					    downwards -> dia_utils:expand_tran_downwards(Tran, Island)
					end}
				       || Tran <- dia_utils:sort_trans(
						    case Dir of
							upwards -> dia_utils:expand_node_id_to_trans_up(NodeId, Island);
							downwards -> dia_utils:expand_node_id_to_trans_down(NodeId, Island)
						    end)]
				      || NodeId <- List]),
    NewNodeTuples = lists:usort(TempNewNodeTuples),
    Path#path{cur_list_nodes = NewNodeTuples,
	      node_ids = [dia_utils:get_nod_ids(element(2, lists:unzip(NewNodeTuples))),
			  List|Rest],
	      depth = N + 1}.


% Effectively joins all the elements in both "equivalent"
% paths provided in the Island
join_path_pair(#path{node_ids = ListOfLists},
	       #path{node_ids = ListOfLists2},
	       Drai) ->
    dia_utils:rebuild_idxs(
    dia_utils:remove_orphan_nodes(
      lists:foldl(
	fun ({X, Y}, AcDrai) -> dia_utils:join_node_pairs(X, Y, AcDrai) end,
	Drai, lists:zip(
		lists:concat(
		  lists:reverse(ListOfLists)),
		lists:concat(
		  lists:reverse(ListOfLists2)))))).

% Takes a list of paths (PathList), and
% it groups it to a list of lists, which
% associates equivalent paths.
% Because there may be several possibilities
% inside a Path, those will be filtered
% when added to the groups.
% A path may be added to several groups.
% We should eventually put a limit to this.
group_paths(PathList) ->
    NewPathList = lists:filter(fun has_not_path_loop/1, PathList),
    NewNewPathList = lists:filter(fun not_reached_top/1, NewPathList),
    utils:group_by(fun compare_paths/2, NewNewPathList).

has_not_path_loop(#path{node_ids = List}) ->
    length(lists:usort(List)) =:= length(List).

not_reached_top(#path{cur_list_nodes = []}) -> false;
not_reached_top(_) -> true.

compare_paths(#path{cur_list_nodes = CurList1, direction = Dir},
	      #path{cur_list_nodes = CurList2, direction = Dir}) ->
    %% case length(CurList1) > 4 of
    %% 	true -> io:format("===~n~p~n~p~n", [lists:sort([{P1, get_node_prop(N1)}|| {P1, N1} <- CurList1]),
    %% 					    lists:sort([{P2, get_node_prop(N2)}|| {P2, N2} <- CurList2])]),
    %% 		io:format("~p ~p~n", [lists:sort([{P1, get_node_prop(N1)}|| {P1, N1} <- CurList1]) =:=	
    %% 	lists:sort([{P2, get_node_prop(N2)}|| {P2, N2} <- CurList2]), Depth]);
    %% 	_ -> ok
    %% end,
    lists:sort([{P1, dia_utils:get_node_prop(N1)}|| {P1, N1} <- CurList1]) =:=	
	lists:sort([{P2, dia_utils:get_node_prop(N2)}|| {P2, N2} <- CurList2]);
compare_paths(_, _) -> false.


% Returns the length of a path as an Integer
path_length(#path{depth = Depth}) -> Depth.

% Creates the basic sturcture for Paths
initialise_path(Node, _Islands, Direction) ->
    #path{cur_list_nodes = [{id, Node}],
	  node_ids = [[dia_utils:get_nod_id(Node)]],
	  depth = 1,
	  ori_node = Node,
	  direction = Direction}.




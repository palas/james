%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Generates procedures for solving dependencies of postconditions
%%% without generating extra control steps
%%% @end
%%% Created :  8 Ene 2015 by Pablo Lamela Seijas
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
-module(used_dep_gen).

-include("records.hrl").

-export([get_paths/1]).

get_paths(Drai) ->
    get_subgraph(Drai).

get_subgraph(Drai) ->
    DepDrai = dia_utils:remove_orphan_nodes(
     		remove_loops_and_control(Drai)),
    Nodes = sets:union(dia_utils:get_top_nodes(DepDrai), dia_utils:get_control_nodes(DepDrai)),
    sets:to_list(Nodes).
%    SubGraph = remove_no_inv_deps(DepDrai, Nodes).

remove_loops_and_control(Drai) ->
    dia_utils:rebuild_idxs(remove_loops(remove_control(Drai))).

remove_loops(Drai) -> remove_arcs(fun remove_control_fun/3, Drai).
remove_control(Drai) -> remove_arcs(fun remove_loops_fun/3, Drai).

remove_arcs(Fun, #drai{darcs = Arcs} = Drai) ->
      Drai#drai{darcs = dict:fold(Fun, dict:new(), Arcs)}.

remove_control_fun(Key, Arc, Dict) ->
    case dia_utils:is_data_dep(Arc) of
	true -> dict:store(Key, Arc, Dict);
	false -> Dict
    end.

remove_loops_fun(Key, #diagram_arc{is_loop = false} = Arc, Dict) ->
    dict:store(Key, Arc, Dict);
remove_loops_fun(_, _, Dict) -> Dict.


% 1. Pick control nodes and root nodes (in terms of data dep minus loops)
% 2. Add to queue inverse dependencies for every root (downwards), - topological sorting
%    in terms of OR or AND of nodes already expanded
%    * OR for diamonds, AND for params.
%    * When no root nodes left stop.
% 3. Generate the functions (in terms of available node dict with bools) from dependency data.
%    If a node is not in the dict
% 4. Check 

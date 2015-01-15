%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Record definitions
%%% @end
%%% Created : 11 Jun 2014 by Pablo Lamela Seijas
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
-record(trace, {pid, traces}).
-record(callback, {kind, depth, is_dynamic, method_name, method_signature, class_signature, params, return, http_request = no, tags, this}).
-record(free_event, {tag}).
-record(value, {type, value, obj_info = none}).
-record(obj_info, {first_time, identifier}).
-record(diagram_node, {id, label, properties = [], is_label_term = false, content, tags = [], http_request = no, cluster = no, class = empty}).
-record(diagram_arc, {id, id_start, id_end, properties = [], content, tags, is_loop = false}).
-record(temp_info, {dependency_index, entity_index, last_entity_id, last_http = none, from_setUp = false}).
-record(drai, {dnodes, darcs, arcsf, arcst}).
-record(path, {cur_list_nodes, node_ids, arc_ids, depth, ori_node, direction}).
-record(config, {remove_bubbles = false, highlight_loops = true,
		 collapse_integers = true, collapse_strings = true,
		 single_file = false, print_node_numbers = false,
		 num_of_islands = 20, % inf = no limit
		 big_k_value = 4, small_k_value = 1, remove_orphan_nodes = true,
		 discard_calls_beginning_with = ["info"], % assert is another useful option
		 remove_nodes_up_from = ["createTest", "getLogger", "run"],
	   this_generator = "run",
		 max_iterations = inf, highlight_last_merge = false}).

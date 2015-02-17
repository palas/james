%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Procedures to parse the input from the JVMTI agent
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
-module(parser_newstruct).

-include("records.hrl").

-export([get_drai/2, get_drai/3, gen_dia_to_files/3, gen_dia_to_files/4, list_traces/1,
	 save_filtered_messages/2, save_filtered_messages/3, drai_to_file/2,
	 gen_eqc/4, gen_eqc/5, gen_dia_to_files_dbg/3, gen_dia_to_files_dbg/4]).

gen_eqc(Pid, N, Path, Module) ->
    Drai = get_drai(Pid, N),
    gen_eqc_aux(Path, Module, Drai).

gen_eqc(Pid, N, Path, Module, PreConfig) ->
    Drai = get_drai(Pid, N, PreConfig),
    gen_eqc_aux(Path, Module, Drai).

gen_eqc_aux(Path, Module, Drai) ->
    template_gen:fun_templates(Drai, Path, Module),
    dep_fsm_gen:gen_dep(Drai, Path, Module),
    check_fsm_gen:gen_checks(Drai, Path, Module),
    used_dep_gen:gen_used_dep(Drai, Path, Module),
    eqc_fsm_gen:gen_eqc(Drai, Path, Module).

get_drai(Pid, N) -> get_drai(Pid, N, #config{}).
get_drai(Pid, N, PreConfig) ->
    Config = PreConfig#config{remove_bubbles = false,
			      highlight_loops = true,
			      collapse_integers = true,
			      collapse_strings = true,
			      single_file = true,
			      num_of_islands = inf,
			      remove_orphan_nodes = true,
			      discard_calls_beginning_with = [],
			      remove_nodes_up_from = []},
    {ONodes, OArcs} = filter_and_gen_dia(fetch_traces(Pid, N), Config),
    [{Nodes, Arcs}] = combinator:combinate(ONodes, OArcs, Config),
    dia_utils:create_drai(Nodes, Arcs).

drai_to_file(File, Drai) ->
    {Nodes, Arcs} = islands:fusion_islands([Drai]),
    file:write_file(File, list_to_binary(
			    lists:flatten(
			      diagram_tool:write_diagram(Nodes, Arcs)))).

save_filtered_messages(Pid,File) ->
    save_filtered_messages(Pid,File,#config{}).

save_filtered_messages(Pid,File,Config) ->
    {ok, Messages} = server:get_messages(Pid),
    NewMessages = [{SPid, filter_non_important(Msg, Config)} || {SPid, Msg} <- Messages],
    file:write_file(File, term_to_binary(NewMessages)).

diagram_to_files(File, List, Config) ->
    Diagrams = make_diagram(List,Config),
    [file:write_file(File ++ integer_to_list(N) ++ ".dot", list_to_binary(lists:flatten(Diagram)))
     || {N, Diagram} <- lists:zip(lists:seq(1,length(Diagrams)), Diagrams)].

make_diagram(List, Config) ->
    {ONodes, OArcs} = filter_and_gen_dia(List, Config),
    [diagram_tool:write_diagram(Nodes, Arcs)
     || {Nodes, Arcs} <- combinator:combinate(ONodes, OArcs, Config)].

filter_and_gen_dia(List, Config) ->
    gen_diagram(filter_non_important(List, Config), Config).

list_traces(Pid) ->
    Traces = get_traces(Pid),
    [{N, length(List)} ||
	{N, {_, List}} <- lists:zip(lists:seq(1, length(Traces)), Traces)].

gen_dia_to_files(Pid,N,File) -> gen_dia_to_files(Pid, N, File, #config{}).

gen_dia_to_files(Pid,N,File,Config) ->
    diagram_to_files(File,fetch_traces(Pid, N),Config).

gen_dia_to_files_dbg(Pid,N,File) -> gen_dia_to_files_dbg(Pid, N, File, #config{}).
gen_dia_to_files_dbg(Pid,N,File,#config{max_iterations = Num} = Config) ->
    try begin
	    gen_dia_to_files(Pid, N, File, #config{max_iterations = count_iterations}),
	    exit(could_not_count_iterations)
	end
    catch
	throw:({count_iterations, Max}) ->
	    gen_dia_to_files_dbg_aux(Pid, N, File, Config, Num, Max)
    end.
gen_dia_to_files_dbg_aux(Pid,N,File,Config,Num,Max) when is_integer(Num) andalso Num < Max ->
    gen_dia_to_files_dbg_aux2(Pid,N,File,Config,1,Num);
gen_dia_to_files_dbg_aux(Pid,N,File,Config,_Num,Max) ->
    gen_dia_to_files_dbg_aux2(Pid,N,File,Config,1,Max).

gen_dia_to_files_dbg_aux2(_Pid,_N,_File,_Config,Max,Max) -> ok;
gen_dia_to_files_dbg_aux2(Pid,N,File,Config,Num,Max) when is_list(File) ->
    Path = (File ++ integer_to_list(Num)),
    ok = file:make_dir(Path),
    gen_dia_to_files(Pid,N,Path ++ "/",Config#config{max_iterations = Num}),
    gen_dia_to_files_dbg_aux2(Pid,N,File,Config,Num + 1,Max);
gen_dia_to_files_dbg_aux2(Pid,N,File,Config,Num,Max) when is_atom(File) ->
    gen_dia_to_files_dbg_aux2(Pid,N,atom_to_list(File),Config,Num,Max).


fetch_traces(Pid, N) -> element(2, hd(get_traces(Pid, [N]))).

get_traces(Pid) -> get_traces(Pid, all).
get_traces(Pid, NList) ->
    {ok, Messages} = server:get_messages(Pid),
    Set = case NList of 
	      all -> sets:new();
	      _ -> sets:from_list(NList)
	  end,
    [{APid, Mes}
     || {N, {APid, Mes}} <-
	    lists:zip(
	      lists:seq(1, length(Messages)),
	      Messages),
	((NList =:= all) orelse (sets:is_element(N, Set)))].

filter_non_important(List, Config) ->
    Set = find_important(List, Config),
    lists:filter(fun (X) -> sets:is_element(X, Set) end, List).

find_important(Traces, Config) ->
    find_important(Traces, sets:new(), dict:new(), Config).
% Index all return values
find_important([(#callback{kind = exit_method,
			   depth = -1,
			   return =
			       #value{type = _,
				      obj_info = #obj_info{identifier = Id}}} = Callback)|Rest],
	       Set, Dict, Config) when is_integer(Id) ->
    find_important(Rest, Set, dict:store(Id, Callback, Dict), Config);
% Find all the entry methods with depth: add them to the list Set
find_important([(#callback{kind = enter_method,
			   depth = Depth,
			   params = Params,
			   this = This}) = Callback|Rest],
	       Set, Dict, Config) when Depth >= 0 andalso Depth =< 1 ->
    NewSet = rec_add_deps(this_to_list(This) ++ Params, Set, Dict),
    find_important(Rest, sets:add_element(Callback, NewSet), Dict, Config);
% Find all the exit method with Depth, add them with their return values to the dictionary Dict
% discard all the callback that don't match
find_important([(#callback{kind = exit_method,
			   depth = Depth,
			   return = Return,
			   method_name = MethodName
			  } = Callback)|Rest],
	       Set, Dict, Config) when Depth >= 0 andalso Depth =< 1 ->
    case starts_with_any(MethodName, Config#config.discard_calls_beginning_with) of
	true -> find_important(Rest, Set, Dict, Config);
	false ->
	    find_important(Rest, sets:add_element(Callback, Set),
			   case Return of
			       #value{type = _,
				      obj_info = #obj_info{identifier = Id}} when is_integer(Id) ->
				   dict:store(Id, Callback, Dict);
			       _ -> Dict
			   end, Config)
    end;
find_important([_|Rest], Set, Dict, Config) -> find_important(Rest, Set, Dict, Config);
find_important([], Set, _, _) -> Set.

starts_with_any(MethodName, List) ->
    lists:any(starts_with_gen(MethodName), List).

starts_with_gen(MethodName) ->
    fun (X) -> starts_with(X, MethodName) end.

starts_with([], _) -> true;
starts_with([H|Rest1], [H|Rest2]) -> starts_with(Rest1, Rest2);
starts_with(_, _) -> false.

% get all their parameters and "this"'s and recursively add their dependencies to
% the Set
rec_add_deps([#value{type = _, obj_info = #obj_info{identifier = Id}}|Rest], Set, Dict) when is_integer(Id) ->
    case dict:find(Id, Dict) of
	{ok, Value} ->
	    case sets:is_element(Value, Set) of
		true -> rec_add_deps(Rest, Set, Dict);
		false -> rec_add_deps(get_dependencies(Value) ++ Rest,
				      sets:add_element(Value, Set), Dict)
	    end;
	error -> rec_add_deps(Rest, Set, Dict)
    end;
rec_add_deps([_|Rest], Set, Dict) -> rec_add_deps(Rest, Set, Dict);
rec_add_deps([], Set, _) -> Set.

get_dependencies(#callback{params = Params,
			   this = This}) ->
    this_to_list(This) ++ Params.

this_to_list(#value{} = Value) -> [Value];
this_to_list(_) -> [].

gen_diagram(List, Config) ->
    TInfo = new_temp_info(Config),
    %% TInfo2 = gen_bas_conn(List, TInfo),
    TInfo2 = gen_bas_conn_aux(List, TInfo), % <-- Only one island
    Entities = element(2, lists:unzip(dict:to_list(TInfo2#temp_info.entity_index))),
    {[Node || #diagram_node{} = Node <- Entities],
     [Arc || #diagram_arc{} = Arc <- Entities]}.

gen_bas_conn_aux(Callbacks, TInfo) ->
    lists:foldl(fun gen_bas_connections/2, TInfo, Callbacks).

% If is enter method we just find and create dependencies
gen_bas_connections(#callback{kind = enter_method} = Callback, TInfo) ->
    Deps = get_dependency_values(Callback), % Deps is a list of values of params
					    % The first param is this a value may
					    % be just an atom (meaning it doesn't exist)
    {TInfo2, _} = fetch_or_create_values(Deps, TInfo), % ResDeps is a list of ids of nodes
						       % maybe just created if the id is negative, it
						       % means it does not exist
%%     {TInfo3, _} = create_usage_values(Deps, TInfo2, dummy), % ResDeps is a list of ids of nodes
%% 						            % maybe just created if the id is negative, it
%% 						            % means it does not exist
    TInfo2;                                     % If is exit method, we add it to the graph
						% if it has dependencies, we link the dependencies
gen_bas_connections(#callback{kind = exit_method} = Callback, TInfo) ->
    Deps = get_dependency_values(Callback), % Deps is a list of values of params
					    % The first param is this a value may
					    % be just an atom (meaning it doesn't exist)
    {TInfo2, [ResThis|ResDeps]} =
	fetch_or_create_values(Deps, TInfo), % ResDeps is a list of ids of nodes
					     % maybe just created if the id is negative, it
					     % means it does not exist
    {TInfo3, Id} = add_call_to_graph(Callback, TInfo2), % Id is the id of the new node
    {TInfo4, [ResThisUsage|ResDepsUsage]} =
	create_usage_values(Deps, TInfo3, Id), % ResDeps is a list of ids of nodes
					       % maybe just created if the id is negative, it
					       % means it does not exist
    {TInfo5, _} = link_this_to_one(ResThis, Id, TInfo4), % Special link for "this" relation
    TInfo6 = link_control_to_one(Callback, Id, TInfo5), % Special link for traversal http control
    TInfo7 = link_all_to_one(ResDeps, Id, TInfo6), % Link each dep with new node
    TInfo8 = link_all_usage_to_one([ResThisUsage|ResDepsUsage], Id, TInfo7), % Link each usage dep with new node
    RetVal = get_return_value(Callback),
    TInfo9 = store_dependency_usage(RetVal, TInfo8, Id, return), % Store the return value as id
					                         % provided it is a value
    store_dependency(RetVal, TInfo9, Id). % Store the return value as id
					  % provided it is a value

new_temp_info(Config) -> #temp_info{dependency_index = dict:new(),
			            usage_dependency_index = dict:new(),
			            entity_index = dict:new(),
			            last_entity_id = 1, config = Config}.

get_dependency_values(#callback{params = Params, this = This}) -> [This|Params].

get_return_value(#callback{return = Return}) -> Return.


fix_classname([_|Rest]) -> fix_classname_aux(Rest).
fix_classname_aux([_El]) -> [];
fix_classname_aux([$/|Rest]) -> [$.|fix_classname_aux(Rest)];
fix_classname_aux([Else|Rest]) -> [Else|fix_classname_aux(Rest)].

gen_label_for_callback(#callback{method_name = "<init>", class_signature = Class}) ->
    "new " ++ fix_classname(Class);
gen_label_for_callback(#callback{method_name = MethodName}) -> MethodName.
gen_label_for_value(#value{} = Value) -> parser_utils:extract_value(Value).

is_static(#callback{is_dynamic = IsDynamic}) ->
    not IsDynamic.

get_tags(#callback{tags = Tags}) -> Tags.

get_httpreqinfo(#callback{http_request = HttpRequest}) -> HttpRequest.

fetch_or_create_values(Deps, TInfo) ->
  swap_tuple(lists:mapfoldl(fun fetch_or_create_value/2, TInfo, Deps)).

gen_par_types(0, _) -> [];
gen_par_types(N, PN) -> [{param, PN}|gen_par_types(N - 1, PN + 1)].
gen_par_types(N) -> [this|gen_par_types(N - 1, 0)].

fetch_or_create_value(Dep, TInfo) when is_atom(Dep) -> {-1, TInfo};
fetch_or_create_value(#value{} = Value, TInfo) ->
    case find_dependency(Value, TInfo) of
	{ok, Id} -> {Id, TInfo};
	error -> {TInfo2, Id} = add_value_to_graph(Value, TInfo),
		 {Id, store_dependency(Value, TInfo2, Id)}
    end.

create_usage_values(Deps, TInfo, Id) ->
  swap_tuple(lists:mapfoldl(fun (A, B) -> create_usage_value(A, B, Id) end, TInfo, lists:zip(gen_par_types(length(Deps)), Deps))).

swap_tuple({A, B}) -> {B, A}.

create_usage_value({_, Dep}, TInfo, _Id) when is_atom(Dep) -> {-1, TInfo};
create_usage_value({Type, #value{} = Value}, TInfo, Id) ->
    LastId = case find_dependency_usage(Value, TInfo) of
		 {ok, IId} -> IId;
		 error -> {ok, IId} = find_dependency(Value, TInfo),
              {result, IId}
	     end,
    {{Type, LastId}, store_dependency_usage(Value, TInfo, Id, Type)}.

%% find_dependency(#value{obj_info = #obj_info{identifier = Id}} = Value,
%% 		#temp_info{dependency_index = Index}) when is_integer(Id) ->
%%     dict:find(parser_utils:extract_id(Value), Index);
find_dependency(#value{} = Value, #temp_info{dependency_index = Index}) ->
    dict:find(parser_utils:extract_id(Value), Index);
find_dependency(_, _) -> {ok, -1}.


find_dependency_usage(#value{} = Value, #temp_info{usage_dependency_index = Index}) ->
    dict:find(parser_utils:extract_id(Value), Index);
find_dependency_usage(_, _) -> {ok, -1}.

%    store_dependency(get_return_value(Callback), Id, TInfo5). % Store the return value as id
% provided it is a value
store_dependency(#value{} = Value, #temp_info{dependency_index = Index} = TInfo, Id) ->
    TInfo#temp_info{dependency_index = dict:store(parser_utils:extract_id(Value), Id, Index)};
store_dependency(_, TInfo, _) -> TInfo.

store_dependency_usage(#value{} = Value, #temp_info{usage_dependency_index = UsageIndex} = TInfo, Type, Id) ->
    TInfo#temp_info{usage_dependency_index = dict:store(parser_utils:extract_id(Value), {Id, Type}, UsageIndex)};
store_dependency_usage(_, TInfo, _, _) -> TInfo.

%    {TInfo3, Id} = add_call_to_graph(Callback, TInfo2), % Id is the id of the new node
add_call_to_graph(Callback, TInfo) ->
    {TInfo2, Id} = get_new_entity_id(TInfo),
    Node = mk_node(Id, gen_label_for_callback(Callback),
		   [rectangle] ++ [{peripheries, 2} || is_static(Callback)],
		   Callback, false, get_tags(Callback), get_httpreqinfo(Callback)),
    TInfo3 = add_to_entity_index(Id, Node, TInfo2),
    {TInfo3, Id}.

add_value_to_graph(Value, TInfo) ->
    {TInfo2, Id} = get_new_entity_id(TInfo),
    Node = mk_node(Id, gen_label_for_value(Value), [ellipse], Value, true, [], no),
    TInfo3 = add_to_entity_index(Id, Node, TInfo2),
    {TInfo3, Id}.

%    TInfo4 = link_this_to_one(ResThis, Id, TInfo3), % Special link for "this" relation
link_this_to_one(-1, _, TInfo) -> {TInfo, -1};
link_this_to_one(Ori, Dest, TInfo) ->
    {TInfo2, Id} = get_new_entity_id(TInfo),
    Arc = mk_arc(Id, Ori, Dest, [dashed], this),
    TInfo3 = add_to_entity_index(Id, Arc, TInfo2),
    {TInfo3, Id}.

which_http_tag([is_before|_]) -> is_before;
which_http_tag([is_test|_]) -> is_test;
which_http_tag([is_after|_]) -> is_after;
which_http_tag([_|Rest]) -> which_http_tag(Rest);
which_http_tag([]) -> none.

link_control_to_one(#callback{depth = 0, tags = Tags}, _, #temp_info{from_setUp = FSU} = TInfo) ->
    case {which_http_tag(Tags), FSU} of
	{is_before, _} -> TInfo#temp_info{from_setUp = true, last_http = none};
	{is_test, true} -> TInfo#temp_info{from_setUp = false};
	{is_test, false} -> TInfo#temp_info{from_setUp = false, last_http = none};
	_ -> TInfo
    end;
link_control_to_one(#callback{http_request = Req}, Id, #temp_info{last_http = none} = OriTempInfo)
  when Req =/= no ->
    TempInfo = tag_entry_point(Id, OriTempInfo),
    TempInfo#temp_info{last_http = Id};
link_control_to_one(#callback{http_request = Req}, Id, #temp_info{last_http = LastId} = TInfo)
  when Req =/= no ->
    {TInfo2, ArcId} = get_new_entity_id(TInfo),
    Arc = mk_arc(ArcId, LastId, Id, [{color, 204, 102, 51}], http_order),
    TInfo3 = add_to_entity_index(ArcId, Arc, TInfo2),
    TInfo3#temp_info{last_http = Id};
link_control_to_one(_, _, TInfo) -> TInfo.

tag_entry_point(Id, #temp_info{entity_index = EIndex} = TempInfo) ->
    Entity = dict:fetch(Id, EIndex),
    UpdatedEntity = Entity#diagram_node{
		      tags = [entry_point|Entity#diagram_node.tags]},
    TempInfo#temp_info{entity_index = dict:store(Id, UpdatedEntity, EIndex)}.

%    TInfo5 = link_all_to_one(ResDeps, Id, TInfo4), % Link each dep with new node
link_all_to_one(Deps, Dest, TInfo) ->
    lists:foldl(fun ({N, X}, TI) -> link_one_to_one_param(X, Dest, TI, N) end, TInfo,
		lists:zip(lists:seq(1, length(Deps)), Deps)).

link_one_to_one_param(Ori, Dest, TInfo, ParamNumber) ->
    {TInfo2, Id} = get_new_entity_id(TInfo),
    Arc = mk_arc(Id, Ori, Dest, [solid], {param, ParamNumber}),
    add_to_entity_index(Id, Arc, TInfo2).

link_all_usage_to_one(Deps, Dest, TInfo) ->
    lists:foldl(fun ({Type, {N, X}}, TI) ->
                        case usage_enabled(TI) of
                                true -> link_one_to_one_usage(X, Dest, TI, N, Type);
                                false -> TI
                        end;
                    (-1, TI) -> TI end, TInfo, Deps).

usage_enabled(#temp_info{config = #config{track_usage = UsageEnabled}}) -> UsageEnabled.

link_one_to_one_usage(Ori, Dest, TInfo, UsageInfo, Type) ->
    {TInfo2, Id} = get_new_entity_id(TInfo),
    Arc = mk_arc(Id, Ori, Dest, [dotted], {usage, Type}, UsageInfo),
    add_to_entity_index(Id, Arc, TInfo2).

mk_node(Id, Label, Properties, Content, IsLabelTerm, Tags, HttpReqInfo) ->
    #diagram_node{id = integer_to_list(Id), label = Label, properties = Properties,
		  is_label_term = IsLabelTerm, content = Content, tags = Tags,
		  http_request = HttpReqInfo}.

mk_arc(Id, IdStart, IdEnd, Properties, Content) -> mk_arc(Id, IdStart, IdEnd, Properties, Content, return).
mk_arc(Id, IdStart, IdEnd, Properties, Content, UsageInfo) ->
    #diagram_arc{id = integer_to_list(Id), id_start = integer_to_list(IdStart),
				 start_type = UsageInfo, id_end = integer_to_list(IdEnd),
				 properties = Properties, content = Content,
				 tags = [{historic_rel, IdStart, Id, IdEnd}]}.

add_to_entity_index(Id, Item, #temp_info{entity_index = Dict} = TInfo) ->
    TInfo#temp_info{entity_index = dict:store(Id, Item, Dict)}.

get_new_entity_id(#temp_info{last_entity_id = LastId} = TInfo) ->
    Id = LastId + 1,
    {TInfo#temp_info{last_entity_id = Id}, Id}.



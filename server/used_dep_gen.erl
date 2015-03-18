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

-export([get_dep_data/1, gen_used_dep/3]).

get_dep_data(Drai) ->
    DepDrai = dia_utils:remove_orphan_nodes(
     		remove_loops_and_control(Drai)),
    TopNodes = dia_utils:get_top_nodes(DepDrai),
    ControlNodes = dia_utils:get_control_nodes(DepDrai),
	{DepDrai, TopNodes, ControlNodes}.

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

gen_used_dep(Drai, Path, ModuleName) ->
   	{DepDrai, _TopNodes, ControlNodes} = get_dep_data(Drai),
		AllNodesDict = Drai#drai.dnodes,
	  RestOfNodesDict = dict:filter(fun (K, _) -> not sets:is_element(K, ControlNodes) end, AllNodesDict),
	  ControlNodesDict = dict:filter(fun (K, _) -> sets:is_element(K, ControlNodes) end, AllNodesDict),
	  %TopNodesDict = dict:filter(fun (K, _) -> sets:is_element(K, TopNodes) end, AllNodesDict),
    ThisModuleName = atom_to_list(ModuleName) ++ "_used_dep",
    {{Prim, OneOfs, Calls}, DepDrai} = dict:fold(fun gen_used_dep_aux/3, {{[], [], []}, DepDrai}, RestOfNodesDict),
    ControlNodesP = dict:fold(fun pack_node_info/3, [], ControlNodesDict),
    file:write_file(Path ++ ThisModuleName ++ ".erl",
		    dep_file(ModuleName, ThisModuleName, {ControlNodesP, Prim, OneOfs, Calls})).
gen_used_dep_aux(Code, #diagram_node{properties = [ellipse|_]} = Rec, {{PrimL, OneOfsL, CallsL}, Drai}) ->
	{{[{prim, Code, Rec, none}|PrimL], OneOfsL, CallsL}, Drai};
gen_used_dep_aux(Code, #diagram_node{properties = [diamond|_]} = Rec,
		 {{PrimL, OneOfsL, CallsL}, Drai}) ->
    {{PrimL, [{oneOf, Code, Rec, dia_utils:parents_codes(Code, Drai, diamond)}|OneOfsL], CallsL}, Drai};
gen_used_dep_aux(Code, #diagram_node{properties = [rectangle|_], content = #callback{params = CPList, this = CThis}} = Rec,
		 {{PrimL, OneOfsL, CallsL}, Drai}) ->
    {{PrimL, OneOfsL, [{acall, Code, Rec, dia_utils:parents_codes(Code, Drai, normal), {CThis, CPList}}|CallsL]}, Drai}.
pack_node_info(Code, Rec, List) -> [{control, Code, Rec, none}|List].


%Fun = fun((Key, Value, AccIn) -> AccOut)

dep_file(ModuleName, ThisModuleName, {ControlNodes, Prim, OneOfs, Calls}) ->
    SyntaxTree = mk_tree(ModuleName, ThisModuleName, {ControlNodes, Prim, OneOfs, Calls}),
    lists:flatten([io_lib:format("~s~n~n", [erl_prettypr:format(T)]) || T <- SyntaxTree]).

mk_tree(ModuleName, ThisModuleName, {ControlNodes, Prim, OneOfs, Calls}) ->
    header(ThisModuleName) ++
	[erl_syntax:function(
	   erl_syntax:atom(used_args_for),
	   control_funcs(ModuleName, ThisModuleName, ControlNodes) ++
	       prim_funcs(ModuleName, ThisModuleName, Prim) ++
	       oneofs_funcs(ModuleName, ThisModuleName, OneOfs) ++
	       call_funcs(ModuleName, ThisModuleName, Calls))].

header(Module) ->
  [erl_syntax:attribute(erl_syntax:atom(module),[erl_syntax:atom(Module)]),
   erl_syntax:attribute(erl_syntax:atom(compile),[erl_syntax:atom(export_all)])
  ].

control_funcs(_ModuleName, _ThisModuleName, Prim) ->
    [erl_syntax:clause(
       [erl_syntax:variable("_Size"),
	erl_syntax:variable("State"),
	erl_syntax:variable("WhatToReturn"),
	erl_syntax:abstract(Code)],
       none,
       [erl_syntax:application(erl_syntax:atom(utils), erl_syntax:atom(control_add),
			       [erl_syntax:variable("State"),
				erl_syntax:variable("WhatToReturn"),
				erl_syntax:abstract(Code)])])
     || {control, Code, _Node, none} <- Prim].

prim_funcs(ModuleName, _ThisModuleName, Prim) ->
    [erl_syntax:clause([erl_syntax:variable("_Size"),
                        erl_syntax:variable("_State"),
			erl_syntax:variable("WhatToReturn"),
			erl_syntax:abstract(Code)],
		       none,
		       [erl_syntax:tuple(
			  [erl_syntax:atom(ok),
			   erl_syntax:tuple(
			     [erl_syntax:atom(jcall),
			      erl_syntax:atom(ModuleName),
			      erl_syntax:atom(actual_callback),
			      erl_syntax:list(
				[erl_syntax:abstract(Code),
				 erl_syntax:variable("WhatToReturn"),
				 map_abstract(template_gen:value_to_map(Val)),
				 erl_syntax:list([])])
			     ])])])
     || {prim, Code, #diagram_node{content = Val}, none} <- Prim].

oneofs_funcs(_ModuleName, _ThisModuleName, OneOfs) ->
    [begin
	 ({A, B} = SplittedNodes) = split_calls(PNodes),
	 FilteredNodes = A ++ B,
	 Args = args_for_with_size(SplittedNodes),
	 erl_syntax:clause(
	   [erl_syntax:variable(utils:underscore_if_ne("Size", FilteredNodes)),
	    erl_syntax:variable(utils:underscore_if_ne("State", PNodes)),
	    erl_syntax:variable("_WhatToReturn"),
	    erl_syntax:abstract(Code)],
	   none,
	   [case PNodes of
		[] -> erl_syntax:atom(error);
		_ ->	erl_syntax:application(
			  erl_syntax:atom(utils), erl_syntax:atom(used_or),
			  [erl_syntax:application(
			     erl_syntax:atom(utils),
			     erl_syntax:atom(normalise_weights),
			     [erl_syntax:list_comp(
				erl_syntax:application(
				  erl_syntax:atom(utils),
				  erl_syntax:atom(add_weights),
				  [erl_syntax:variable("State"),
				   erl_syntax:variable("Node"),
				   erl_syntax:application(
				     erl_syntax:atom(used_args_for),
				     [erl_syntax:variable("NewSize"),
				      erl_syntax:variable("State"),
				      erl_syntax:variable("WhatToReturn"),
				      erl_syntax:variable("Node")])]),
				[erl_syntax:generator(
				   erl_syntax:tuple(
				     [erl_syntax:variable("NewSize"),
				      erl_syntax:variable("WhatToReturn"),
				      erl_syntax:variable("Node")]),
				   Args
				  )])])])
	    end])
     end
     || {oneOf, Code, #diagram_node{http_request = no}, PNodes} <- OneOfs].

check_sig({A, _}, {B, _}) when (is_list(A) andalso is_atom(B)) ->
    false;
check_sig({A, _}, {B, _}) when (is_atom(A) andalso is_tuple(B)) ->
    false;
check_sig({_, A}, {_, B}) when (length(A) =/= length(B)) ->
    false;
check_sig({_Params, _This}, {_EParams, _EThis}) ->
    true.
call_funcs(ModuleName, _ThisModuleName, Calls) ->
	[begin
	     IsThis = lists:member(this, Node#diagram_node.tags),
	     SigOk = check_sig({This, Params}, {EThis, EParams}),
	     erl_syntax:clause(
	       [erl_syntax:variable(utils:underscore_if_true("Size", (IsThis orelse (not SigOk)) orelse (is_atom(This) andalso (Params =:= [])))),
		erl_syntax:variable(utils:underscore_if_true("State", IsThis orelse (not SigOk))),
		erl_syntax:variable(utils:underscore_if_true("WhatToReturn", IsThis orelse (not SigOk))),
		erl_syntax:abstract(Code)],
	       none,
	       [case IsThis of
		    false ->
			case SigOk of
			    true -> erl_syntax:case_expr(
				      erl_syntax:application(
					erl_syntax:atom("utils"),erl_syntax:atom("control_add"),
					[erl_syntax:variable("State"),erl_syntax:variable("WhatToReturn"),erl_syntax:abstract(Code)]),
				      [erl_syntax:clause([erl_syntax:atom(error)],
							 none,
							 [erl_syntax:match_expr(
							    erl_syntax:variable("Params"),
							    erl_syntax:tuple([this_call(This),
									      gen_calls_for_with_size_normal(call, Params)])),
							  erl_syntax:match_expr(
							    erl_syntax:variable("CParams"),
							    erl_syntax:application(
							      erl_syntax:atom("utils"), erl_syntax:atom("used_and_res"),
							      [erl_syntax:variable("Params")])),
							  erl_syntax:application(erl_syntax:atom("utils"),erl_syntax:atom("used_and_fix"),
										 [erl_syntax:tuple(
										    [erl_syntax:atom(jcall),
										     erl_syntax:atom(ModuleName),
										     erl_syntax:atom(actual_callback),
										     erl_syntax:list(
										       [erl_syntax:abstract(Code),
											erl_syntax:variable("WhatToReturn"),
											map_abstract(template_gen:callback_to_map(Callback)),
											erl_syntax:variable("CParams")
										       ])]),
										  erl_syntax:variable("CParams")])]),
				       erl_syntax:clause([erl_syntax:variable("Else")],none,[erl_syntax:variable("Else")])]);
			    false -> erl_syntax:atom(error)
			end;
		    true -> erl_syntax:tuple([erl_syntax:atom(ok), erl_syntax:abstract(this)])
		end])
	 end
	 || {acall, Code, (#diagram_node{content = Callback} = Node), {This, Params}, {EThis, EParams}} <- Calls].

this_call({CallType, List}) when is_list(List) -> gen_calls_for(call, {CallType, List});
this_call(Else) -> erl_syntax:abstract(Else).


args_for_with_size({Normal, []}) -> gen_calls_for_with_size_normal(args, Normal);
args_for_with_size({[], Loop}) -> gen_calls_for_with_size_normal(args, Loop);
args_for_with_size({Normal, Loop}) ->
    erl_syntax:infix_expr(gen_calls_for_with_size_normal(args, Normal),
			  erl_syntax:operator("++"),
			  gen_calls_for_with_size_loop(args, Loop)).
gen_calls_for_with_size_loop(OutMode, [Loop]) ->
	erl_syntax:list_comp(gen_calls_for_aux(OutMode, Loop, erl_syntax:infix_expr(erl_syntax:variable("Size"),
								       erl_syntax:operator("-"), erl_syntax:integer(1))),
			     [erl_syntax:infix_expr(erl_syntax:variable("Size"),
						    erl_syntax:operator(">"),
						    erl_syntax:integer(0))]);
gen_calls_for_with_size_loop(OutMode, [Loop | Rest]) ->
	erl_syntax:infix_expr(gen_calls_for_with_size_loop(OutMode, [Loop]),
			      erl_syntax:operator("++"),
			      gen_calls_for_with_size_loop(OutMode, Rest)).
gen_calls_for_with_size_normal(OutMode, List) ->
	erl_syntax:list(lists:map(fun (X) -> gen_calls_for(OutMode, X) end, List)).

gen_calls_for(OutMode, {CallType, Node}) ->
	gen_calls_for_aux(OutMode, {CallType, Node}, erl_syntax:variable("Size")).
calls_for_aux({CallType, Node}, Size) ->
	erl_syntax:application(
	  erl_syntax:atom(used_args_for),
	  [Size, erl_syntax:variable("State"),
	   erl_syntax:abstract(CallType),
	   erl_syntax:string(Node)]).
args_for_aux({CallType, Node}, Size) ->
	erl_syntax:tuple([Size, erl_syntax:abstract(CallType),
			  erl_syntax:string(Node)]).
gen_calls_for_aux(args, {CallType, Node}, Size) ->
	args_for_aux({CallType, Node}, Size);
gen_calls_for_aux(call, {CallType, Node}, Size) ->
	calls_for_aux({CallType, Node}, Size).

split_calls(List) -> split_calls(List, {[], []}).
split_calls([], Tuple) -> Tuple;
split_calls([{best, NodeId}|Rest], {Normal, Loop}) ->
    split_calls(Rest, {[NodeId|Normal], Loop});
split_calls([{normal, NodeId}|Rest], {Normal, Loop}) ->
    split_calls(Rest, {Normal, [NodeId|Loop]});
split_calls([{dead_end, _}|Rest], {Normal, Loop}) ->
    split_calls(Rest, {Normal, Loop}).

map_abstract(List) when is_list(List) ->
    case io_lib:printable_list(List) of
	true -> erl_syntax:abstract(List);
	false -> erl_syntax:list(lists:map(fun map_abstract/1, List))
    end;
map_abstract(Map) when is_map(Map) ->
    erl_syntax:map_expr(
      [erl_syntax:map_field_assoc(erl_syntax:abstract(Key),
				  map_abstract(Value))
        || {Key, Value} <- maps:to_list(Map)]);
map_abstract(Else) -> erl_syntax:abstract(Else).



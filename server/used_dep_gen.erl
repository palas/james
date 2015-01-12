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
%    find_ctrl_deps(TopNodes, ControlNodes, DepDict, Drai).
%    SubGraph = remove_no_inv_deps(DepDrai, Nodes).

%% {function,1,is_solvable,2,
%%               [{clause,1,
%%                        [{integer,1,5432},{var,1,'AvailableCtrlNodes'}],
%%                        [],
%%                        [{op,1,'orelse',
%%                             {call,1,
%%                                   {atom,1,is_solvable},
%%                                   [{integer,1,123},
%%                                    {var,1,'AvailableCtrlNodes'}]},
%%                             {call,1,
%%                                   {atom,1,is_solvable},
%%                                   [{integer,1,321},
%%                                    {var,1,'AvailableCtrlNodes'}]}}]}]}

%% {function,1,is_solvable,2,
%%               [{clause,1,
%%                        [{integer,1,5432},{var,1,'AvailableCtrlNodes'}],
%%                        [],
%%                        [{op,1,'andalso',
%%                             {call,1,
%%                                   {atom,1,is_solvable},
%%                                   [{integer,1,123},
%%                                    {var,1,'AvailableCtrlNodes'}]},
%%                             {call,1,
%%                                   {atom,1,is_solvable},
%%                                   [{integer,1,321},
%%                                    {var,1,'AvailableCtrlNodes'}]}}]}]}


%% {function,1,is_solvable,2,
%%               [{clause,1,
%%                        [{integer,1,5432},{var,1,'AvailableCtrlNodes'}],
%%                        [],
%%                        [{atom,1,true}]}]}


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

gen_used_dep(Drai, Path, ModuleName) ->
   	{DepDrai, TopNodes, ControlNodes} = get_dep_data(Drai),
	  Union = sets:union(TopNodes, ControlNodes),
		AllNodesDict = Drai#drai.dnodes,
	  RestOfNodesDict = dict:filter(fun (K, _) -> not sets:is_element(K, Union) end, AllNodesDict),
	  ControlNodesDict = dict:filter(fun (K, _) -> sets:is_element(K, ControlNodes) end, AllNodesDict),
	  TopNodesDict = dict:filter(fun (K, _) -> sets:is_element(K, TopNodes) end, AllNodesDict),
    ThisModuleName = atom_to_list(ModuleName) ++ "_used_dep",
    {{[], OneOfs, Calls}, DepDrai} = dict:fold(fun gen_used_dep_aux/3, {{[], [], []}, DepDrai}, RestOfNodesDict),
    ControlNodesP = dict:fold(fun pack_node_info/3, [], ControlNodesDict),
	TopNodesDictP = dict:fold(fun pack_node_info/3, [], TopNodesDict),
    file:write_file(Path ++ ThisModuleName ++ ".erl",
		    dep_file(ModuleName, ThisModuleName, {ControlNodesP, TopNodesDictP, OneOfs, Calls})).
gen_used_dep_aux(Code, #diagram_node{properties = [ellipse|_]} = Rec, {{PrimL, OneOfsL, CallsL}, Drai}) ->
	{{[{prim, Code, Rec, none}|PrimL], OneOfsL, CallsL}, Drai};
gen_used_dep_aux(Code, #diagram_node{properties = [diamond|_]} = Rec, {{PrimL, OneOfsL, CallsL}, Drai}) ->
    {{PrimL, [{oneOf, Code, Rec, parents_codes(Code, Drai, diamond)}|OneOfsL], CallsL}, Drai};
gen_used_dep_aux(Code, #diagram_node{properties = [rectangle|_]} = Rec, {{PrimL, OneOfsL, CallsL}, Drai}) ->
    {{PrimL, OneOfsL, [{acall, Code, Rec, parents_codes(Code, Drai, normal)}|CallsL]}, Drai}.
pack_node_info(Code, Rec, List) -> [{control, Code, Rec, none}|List].

parents_codes(Code, Drai, Mode) ->
    sort_codes(Drai, dia_utils:expand_node_id_to_trans_up(Code, Drai), Mode).

sort_codes(Drai, Codes, Mode) ->
    case {lists:dropwhile(fun (#diagram_arc{content = this}) -> false;
			      (#diagram_arc{content = {param, _}}) -> false;
			      (_) -> true end,
			  utils:sort_using(fun code_sorter/1, Codes)), Mode} of
	{[#diagram_arc{id_start = Id, content = this}|Rest], normal} -> {Id, clean_arcs(normal, Drai, Rest)};
	{Rest, normal} -> {static, clean_arcs(normal, Drai, Rest)};
	{Rest, diamond} -> clean_arcs(diamond, Drai, Rest)
    end.

clean_arcs(normal, _Drai, List) -> lists:map(fun clean_arcs/1, List);
clean_arcs(diamond, Drai, List) -> clean_arcs_diamond(Drai, List).
clean_arcs(#diagram_arc{id_start = Id}) -> Id.

clean_arcs_diamond(_Drai, List) ->
	[{case Loop of true -> loop; false -> normal end, Start}
	|| #diagram_arc{id_start = Start, is_loop = Loop} <- List].

code_sorter(#diagram_arc{content = this}) -> -1;
code_sorter(#diagram_arc{content = {param, N}}) -> N;
code_sorter(_) -> -2.

%Fun = fun((Key, Value, AccIn) -> AccOut)

dep_file(ModuleName, ThisModuleName, {ControlNodes, Prim, OneOfs, Calls}) ->
    SyntaxTree = mk_tree(ModuleName, ThisModuleName, {ControlNodes, Prim, OneOfs, Calls}),
    lists:flatten([io_lib:format("~s~n~n", [erl_prettypr:format(T)]) || T <- SyntaxTree]).

mk_tree(ModuleName, ThisModuleName, {ControlNodes, Prim, OneOfs, Calls}) ->
    header(ThisModuleName) ++
	[erl_syntax:function(
	  erl_syntax:atom(args_for),
		control_funcs(ModuleName, ThisModuleName, ControlNodes) ++
	  		prim_funcs(ModuleName, ThisModuleName, Prim) ++
	      oneofs_funcs(ModuleName, ThisModuleName, OneOfs) ++
	      call_funcs(ModuleName, ThisModuleName, Calls))].

header(Module) ->
  [erl_syntax:attribute(erl_syntax:atom(module),[erl_syntax:atom(Module)]),
   erl_syntax:attribute(erl_syntax:atom(compile),[erl_syntax:atom(export_all)])
  ].

control_funcs(ModuleName, _ThisModuleName, Prim) ->
	[erl_syntax:clause([erl_syntax:variable("_Size"),
		erl_syntax:variable("_State"),
		erl_syntax:abstract(Code)],
		none,
		[erl_syntax:tuple(
			[erl_syntax:atom(jcall),
				erl_syntax:atom(ModuleName),
				erl_syntax:atom(actual_callback),
				erl_syntax:list(
					[erl_syntax:abstract(Code),
						erl_syntax:list([])])
			])])
		|| {control, Code, Node, none} <- Prim].

prim_funcs(ModuleName, _ThisModuleName, Prim) ->
    [erl_syntax:clause([erl_syntax:variable("_Size"),
			erl_syntax:variable("_State"),
			erl_syntax:abstract(Code)],
		       none,
		       [erl_syntax:tuple(
			  [erl_syntax:atom(jcall),
			   erl_syntax:atom(ModuleName),
			   erl_syntax:atom(actual_callback),
			   erl_syntax:list(
			     [erl_syntax:abstract(Code),
			      map_abstract(template_gen:value_to_map(Val)),
			      erl_syntax:list([])])
			  ])])
     || {prim, Code, #diagram_node{content = Val}, none} <- Prim].

oneofs_funcs(_ModuleName, _ThisModuleName, OneOfs) -> 
    [erl_syntax:clause([
			erl_syntax:variable(underscore_if_ne("Size", PNodes)),
			erl_syntax:variable(underscore_if_ne("State", PNodes)),
			erl_syntax:abstract(Code)],
		       none,
		       [erl_syntax:application(
			  erl_syntax:atom(oneof),
			  [calls_for_with_size(PNodes)])])
     || {oneOf, Code, #diagram_node{http_request = no}, PNodes} <- OneOfs].
call_funcs(ModuleName, _ThisModuleName, Calls) ->
	[begin
		 UnderscoreIfNe = (case (This) of
												 static -> [];
												 Else -> Else
											 end ++ Params),
		 erl_syntax:clause([erl_syntax:variable(underscore_if_ne("Size", UnderscoreIfNe)),
			 erl_syntax:variable(underscore_if_ne("State", UnderscoreIfNe)),
			 erl_syntax:abstract(Code)],
			 none,
			 [erl_syntax:tuple(
				 [erl_syntax:atom(jcall),
					 erl_syntax:atom(ModuleName),
					 erl_syntax:atom(actual_callback),
					 erl_syntax:list(
						 [erl_syntax:abstract(Code),
							 map_abstract(template_gen:callback_to_map(Callback)),
							 erl_syntax:tuple([this_call(This),
								 calls_for_with_size_normal(Params)])
						 ])])])
	 end
		|| {acall, Code, #diagram_node{content = Callback}, {This, Params}} <- Calls].

underscore_if_ne(Var, []) -> [$_|Var];
underscore_if_ne(Var, _) -> Var.

this_call(List) when is_list(List) -> calls_for(List);
this_call(Else) -> erl_syntax:abstract(Else).

calls_for_with_size(List) ->
	case split_calls(List) of
		{Normal, []} -> calls_for_with_size_normal(Normal);
		{[], Loop} -> calls_for_with_size_normal(Loop);
		{Normal, Loop} -> erl_syntax:infix_expr(calls_for_with_size_normal(Normal),
			erl_syntax:operator("++"),
			calls_for_with_size_loop(Loop))
	end.
calls_for_with_size_loop([Loop]) ->
	erl_syntax:list_comp(calls_for_aux(Loop, erl_syntax:infix_expr(erl_syntax:variable("Size"),
		erl_syntax:operator("-"), erl_syntax:integer(1))),
		[erl_syntax:infix_expr(erl_syntax:variable("Size"),
			erl_syntax:operator(">"),
			erl_syntax:integer(0))]);
calls_for_with_size_loop([Loop | Rest]) ->
	erl_syntax:infix_expr(calls_for_with_size_loop([Loop]),
		erl_syntax:operator("++"),
		calls_for_with_size_loop(Rest)).
calls_for_with_size_normal(List) ->
	erl_syntax:list(lists:map(fun calls_for/1, List)).

calls_for(Node) -> calls_for_aux(Node, erl_syntax:variable("Size")).
calls_for_aux(Node, Size) -> erl_syntax:application(
		      erl_syntax:atom(args_for_op),
		      [Size, erl_syntax:variable("State"),
		       erl_syntax:string(Node)]).

split_calls(List) -> split_calls(List, {[], []}).
split_calls([], Tuple) -> Tuple;
split_calls([{normal, NodeId}|Rest], {Normal, Loop}) ->
	split_calls(Rest, {[NodeId|Normal], Loop});
split_calls([{loop, NodeId}|Rest], {Normal, Loop}) ->
	split_calls(Rest, {Normal, [NodeId|Loop]}).

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


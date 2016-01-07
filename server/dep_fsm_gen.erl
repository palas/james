%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Generates a file with the data dependency information for use
%%% with EQC
%%% @end
%%% Created : 12 Nov 2014 by Pablo Lamela Seijas
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
-module(dep_fsm_gen).

-include("records.hrl").

-export([gen_dep/3, gen_dep_ann/3, map_abstract/1]).

gen_dep(Drai, Path, ModuleName) -> gen_dep_aux(Drai, Path, ModuleName, false).
gen_dep_ann(Drai, Path, ModuleName) -> gen_dep_aux(Drai, Path, ModuleName, true).

gen_dep_aux(#drai{dnodes = DNodes} = Drai, Path, ModuleName, Annotate) ->
    ThisModuleName = atom_to_list(ModuleName) ++ "_dep",
    {{Prim, OneOfs, Calls}, Drai} = dict:fold(fun gen_dep_aux2/3, {{[], [], []}, Drai}, DNodes),
    Hash = html_utils:gen_hash(),
    file:write_file(Path ++ ThisModuleName ++ case Annotate of
						  false -> ".erl";
						  true -> ".html"
					      end,
		    dep_file(ModuleName, ThisModuleName, {Prim, OneOfs, Calls},
			     Annotate, Hash)).
gen_dep_aux2(Code, #diagram_node{properties = [ellipse|_]} = Rec, {{PrimL, OneOfsL, CallsL}, Drai}) ->
    {{[{prim, Code, Rec, none}|PrimL], OneOfsL, CallsL}, Drai};
gen_dep_aux2(Code, #diagram_node{properties = [diamond|_]} = Rec, {{PrimL, OneOfsL, CallsL}, Drai}) ->
    {{PrimL, [{oneOf, Code, Rec, dia_utils:parents_codes(Code, Drai, diamond)}|OneOfsL], CallsL}, Drai};
gen_dep_aux2(Code, #diagram_node{properties = [rectangle|_]} = Rec, {{PrimL, OneOfsL, CallsL}, Drai}) ->
    {{PrimL, OneOfsL, [{acall, Code, Rec, dia_utils:parents_codes(Code, Drai, normal)}|CallsL]}, Drai}.

%Fun = fun((Key, Value, AccIn) -> AccOut)

dep_file(ModuleName, ThisModuleName, {Prim, OneOfs, Calls}, Annotate, Hash) ->
    SyntaxTree = mk_tree(ModuleName, ThisModuleName, {Prim, OneOfs, Calls}),
    lists:flatten(
      io_lib:format(
	"~s~n",
	[[html_utils:fix_annotations(
	    io_lib:format(
	      "~s~n~n", [erl_prettypr:format(T, html_utils:add_hook(Annotate, Hash))]),
	    Annotate, Hash, "Dependency tree", "")
	  || T <- SyntaxTree]])).

mk_tree(ModuleName, ThisModuleName, {Prim, OneOfs, Calls}) ->
    header(ThisModuleName) ++
	[reuse_fun(ModuleName),
	 erl_syntax:function(
	  erl_syntax:atom(args_for),
	  prim_funcs(ModuleName, ThisModuleName, Prim) ++
	       oneofs_funcs(ModuleName, ThisModuleName, OneOfs) ++
	       call_funcs(ModuleName, ThisModuleName, Calls))].

reuse_fun(_Module) ->
    erl_syntax:function(
      erl_syntax:atom(args_for_op),
      [erl_syntax:clause(
	 [erl_syntax:variable("Size"),
	  erl_syntax:variable("WhatToReturn"),
	  erl_syntax:match_expr(
	    erl_syntax:tuple(
	      [erl_syntax:atom(empty),
	       erl_syntax:atom(empty)]),
	    erl_syntax:variable("State")),
	  erl_syntax:variable("Code")],
	 [],
	 [erl_syntax:macro(
	    erl_syntax:variable("LAZY"),
	    [erl_syntax:application(
	       erl_syntax:atom(args_for),
	       [erl_syntax:variable("Size"),
		erl_syntax:variable("WhatToReturn"),
		erl_syntax:variable("State"),
		erl_syntax:variable("Code")])])]),
       erl_syntax:clause(
	 [erl_syntax:variable("Size"),
	  erl_syntax:variable("WhatToReturn"),
	  erl_syntax:match_expr(
	    erl_syntax:tuple(
	      [erl_syntax:tuple(
		 [erl_syntax:underscore(),
		  erl_syntax:variable("SymState")]),
	       erl_syntax:variable("RawState")]),
	    erl_syntax:variable("State")),
	  erl_syntax:variable("Code")],
	 [],
	 [erl_syntax:case_expr(
	    erl_syntax:application(
	      erl_syntax:module_qualifier(
		erl_syntax:atom("utils"),
		erl_syntax:atom(get_instances_of_sym)),
	      [erl_syntax:variable("Code"),
	       erl_syntax:variable("WhatToReturn"),
	       erl_syntax:variable("SymState"),
	       erl_syntax:variable("RawState")]),
	    [erl_syntax:clause(
	       [erl_syntax:nil()],
	       [],
	       [erl_syntax:application(
		  erl_syntax:atom(args_for),
		  [erl_syntax:variable("Size"),
		   erl_syntax:variable("WhatToReturn"),
		   erl_syntax:variable("State"),
		   erl_syntax:variable("Code")])]),
	     erl_syntax:clause(
	       [erl_syntax:variable("List")],
	       [],
	       [erl_syntax:macro(
		  erl_syntax:variable("LAZY"),
		  [erl_syntax:application(
		     erl_syntax:atom(frequency),
		     [erl_syntax:infix_expr(
			erl_syntax:application(
			  erl_syntax:atom(utils),
			  erl_syntax:atom(set_weights),
			  [erl_syntax:integer(3),
			   erl_syntax:variable("List")]),
			erl_syntax:operator("++"),
			erl_syntax:application(
			  erl_syntax:atom(utils),
			  erl_syntax:atom(set_weights),
			  [erl_syntax:integer(1),
			   erl_syntax:list(
			     [erl_syntax:application(
				erl_syntax:atom(args_for),
				[erl_syntax:variable("Size"),
				 erl_syntax:variable("WhatToReturn"),
				 erl_syntax:variable("State"),
				 erl_syntax:variable("Code")])])]))])])])])])]).

header(Module) ->
  [erl_syntax:attribute(erl_syntax:atom(module),[erl_syntax:atom(Module)]),
   mkinclude("eqc/include/eqc.hrl"),
   erl_syntax:attribute(erl_syntax:atom(compile),[erl_syntax:atom(export_all)])
  ].

mkinclude(String) ->
  erl_syntax:attribute(erl_syntax:atom(include_lib),[erl_syntax:string(String)]).

prim_funcs(ModuleName, _ThisModuleName, Prim) ->
    [erl_syntax:clause([erl_syntax:variable("_Size"),
			erl_syntax:variable("WhatToReturn"),
			erl_syntax:variable("_State"),
			erl_syntax:abstract(Code)],
		       none,
		       [erl_syntax:tuple(
			  [erl_syntax:atom(jcall),
			   erl_syntax:atom(ModuleName),
			   erl_syntax:atom(actual_callback),
			   erl_syntax:list(
			     [erl_syntax:abstract(Code),
			      erl_syntax:variable("WhatToReturn"),
			      map_abstract(template_gen:value_to_map(Val)),
			      erl_syntax:list([])])
			  ])])
     || {prim, Code, #diagram_node{content = Val}, none} <- Prim].

oneofs_funcs(_ModuleName, _ThisModuleName, OneOfs) ->
    [begin
	 ({A, B} = SplittedCalls) = split_calls(PNodes),
	 FilteredCalls = A ++ B,
	 Calls = calls_for_with_size(SplittedCalls, create_hook_fun(Code, Content)),
	 erl_syntax:clause([
			    erl_syntax:variable(utils:underscore_if_ne("Size", FilteredCalls)),
			    erl_syntax:variable("_WhatToReturn"),
			    erl_syntax:variable(utils:underscore_if_ne("State", FilteredCalls)),
			    erl_syntax:abstract(Code)],
			   none,
			   [erl_syntax:macro(
			      erl_syntax:variable("LAZY"),
			      [erl_syntax:application(
				 erl_syntax:atom(oneof),
				 [Calls])])])
     end
     || {oneOf, Code, #diagram_node{http_request = no, content = Content},
	 PNodes} <- OneOfs].
call_funcs(ModuleName, _ThisModuleName, Calls) ->
    [
     begin
	 IsThis = lists:member(this, Node#diagram_node.tags),
	 UnderscoreIfNe = (case (This) of
			       static -> [];
			       Else -> [Else]
			   end ++ Params),
	 erl_syntax:clause(
	   [erl_syntax:variable(utils:underscore_if_true(utils:underscore_if_ne("Size", UnderscoreIfNe), IsThis)),
	    erl_syntax:variable(utils:underscore_if_true("WhatToReturn", IsThis)),
	    erl_syntax:variable(utils:underscore_if_true(utils:underscore_if_ne("State", UnderscoreIfNe), IsThis)),
	    erl_syntax:abstract(Code)],
	   none,
	   [case lists:member(this, Node#diagram_node.tags) of
		false ->
		    erl_syntax:tuple(
		      [erl_syntax:atom(jcall),
		       erl_syntax:atom(ModuleName),
		       erl_syntax:atom(actual_callback),
		       erl_syntax:list(
			 [erl_syntax:abstract(Code),
			  erl_syntax:variable("WhatToReturn"),
			  map_abstract(template_gen:callback_to_map(Callback)),
			  erl_syntax:tuple([this_call(This, create_hook_fun(Code, this)),
					    calls_for_with_size_normal(
					      Params,
					      create_partial_hook_fun(Code))])
			 ])]);
		true -> erl_syntax:abstract(this)
	    end])
     end || {acall, Code, (#diagram_node{content = Callback} = Node), {This, Params}} <- Calls].

create_hook_fun(Code, Content) ->
    complete_hook_fun(Content, create_partial_hook_fun(Code)).
create_partial_hook_fun(Code) ->
    {partial, fun (Content) ->
		      fun (X, Ori) ->
			      add_ann(X, Ori, Code, Content)
		      end
	      end}.
complete_hook_fun(Pos, {partial, HookFun}) -> HookFun(Pos);
complete_hook_fun(_, HookFun) -> HookFun.

add_ann(AST, Ori, Code, Content) ->
    erl_syntax:add_ann({create_anchor, serialise_content(Content) ++ "-"
			++ Ori ++ "-" ++ Code}, AST).

serialise_content(this) -> "this";
serialise_content({param, N}) -> "param" ++ integer_to_list(N).

this_call({_, List} = T, HookFun) when is_list(List) -> calls_for(T, HookFun);
this_call(Else, _HookFun) -> erl_syntax:abstract(Else).

calls_for_with_size({Normal, []}, HookFun) -> calls_for_with_size_normal(Normal, HookFun);
calls_for_with_size({[], Loop}, HookFun) -> calls_for_with_size_normal(Loop, HookFun);
calls_for_with_size({Normal, Loop}, HookFun) ->
    erl_syntax:infix_expr(calls_for_with_size_normal(Normal, HookFun),
			  erl_syntax:operator("++"),
			  calls_for_with_size_loop(Loop, HookFun)).

calls_for_with_size_loop([Loop],HookFun) ->
	erl_syntax:list_comp(calls_for_aux(Loop, erl_syntax:infix_expr(erl_syntax:variable("Size"),
	       erl_syntax:operator("-"), erl_syntax:integer(1)), HookFun),
		[erl_syntax:infix_expr(erl_syntax:variable("Size"),
				       erl_syntax:operator(">"),
				       erl_syntax:integer(0))]);

calls_for_with_size_loop([Loop | Rest], HookFun) ->
	erl_syntax:infix_expr(calls_for_with_size_loop([Loop], HookFun),
		erl_syntax:operator("++"),
		calls_for_with_size_loop(Rest, HookFun)).

calls_for_with_size_normal(List, HookFun) ->
	erl_syntax:list(lists:map(fun (X) -> calls_for(X, HookFun) end, numerate(List))).

numerate(X) -> numerate(1, X).
numerate(_N, []) -> [];
numerate(N, [H|T]) -> [{N, H}|numerate(N + 1, T)].

calls_for({Pos, {WhatToReturn, Node}},HookFun) when is_integer(Pos) ->
    calls_for_aux({WhatToReturn, Node},erl_syntax:variable("Size"), complete_hook_fun({param, Pos}, HookFun));
calls_for({WhatToReturn, Node},HookFun) ->
    calls_for_aux({WhatToReturn, Node},erl_syntax:variable("Size"), HookFun).
calls_for_aux({WhatToReturn, Node},Size,HookFun) ->
    HookFun(erl_syntax:application(
	      erl_syntax:atom(args_for_op),
	      [Size, erl_syntax:abstract(WhatToReturn), erl_syntax:variable("State"),
	       erl_syntax:string(Node)]), Node).

split_calls(List) -> split_calls(List, {[], []}).
split_calls([], Tuple) -> Tuple;
split_calls([{best, NodeId}|Rest], {Normal, Loop}) ->
    split_calls(Rest, {[NodeId|Normal], Loop});
split_calls([{normal, NodeId}|Rest], {Normal, Loop}) ->
    split_calls(Rest, {Normal, [NodeId|Loop]});
split_calls([{dead_end, _NodeId}|Rest], {Normal, Loop}) ->
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


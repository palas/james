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

-export([gen_dep/3, map_abstract/1]).

gen_dep(#drai{dnodes = DNodes} = Drai, Path, ModuleName) ->
    ThisModuleName = atom_to_list(ModuleName) ++ "_dep",
    {{Prim, OneOfs, Calls}, Drai} = dict:fold(fun gen_dep_aux/3, {{[], [], []}, Drai}, DNodes),
    file:write_file(Path ++ ThisModuleName ++ ".erl",
		    dep_file(ModuleName, ThisModuleName, {Prim, OneOfs, Calls})).
gen_dep_aux(Code, #diagram_node{properties = [ellipse|_]} = Rec, {{PrimL, OneOfsL, CallsL}, Drai}) ->
    {{[{prim, Code, Rec, none}|PrimL], OneOfsL, CallsL}, Drai};
gen_dep_aux(Code, #diagram_node{properties = [diamond|_]} = Rec, {{PrimL, OneOfsL, CallsL}, Drai}) ->
    {{PrimL, [{oneOf, Code, Rec, parents_codes(Code, Drai, diamond)}|OneOfsL], CallsL}, Drai};
gen_dep_aux(Code, #diagram_node{properties = [rectangle|_]} = Rec, {{PrimL, OneOfsL, CallsL}, Drai}) ->
    {{PrimL, OneOfsL, [{acall, Code, Rec, parents_codes(Code, Drai, normal)}|CallsL]}, Drai}.

parents_codes(Code, Drai, Mode) ->
    sort_codes(dia_utils:expand_node_id_to_trans_up(Code, Drai), Mode).

sort_codes(Codes, Mode) ->
    case {lists:dropwhile(fun (#diagram_arc{content = this}) -> false;
			      (#diagram_arc{content = {param, _}}) -> false;
			      (_) -> true end,
			  utils:sort_using(fun code_sorter/1, Codes)), Mode} of
	{[#diagram_arc{id_start = Id, content = this}|Rest], normal} -> {Id, clean_arcs(Rest)};
	{Rest, normal} -> {static, clean_arcs(Rest)};
	{Rest, diamond} -> clean_arcs(Rest)
    end.

clean_arcs(List) when is_list(List) -> lists:map(fun clean_arcs/1, List);
clean_arcs(#diagram_arc{id_start = Id}) -> Id.

code_sorter(#diagram_arc{content = this}) -> -1;
code_sorter(#diagram_arc{content = {param, N}}) -> N;
code_sorter(_) -> -2.

%Fun = fun((Key, Value, AccIn) -> AccOut)

dep_file(ModuleName, ThisModuleName, {Prim, OneOfs, Calls}) ->
    SyntaxTree = mk_tree(ModuleName, ThisModuleName, {Prim, OneOfs, Calls}),
    lists:flatten([io_lib:format("~s~n~n", [erl_prettypr:format(T)]) || T <- SyntaxTree]).

mk_tree(ModuleName, ThisModuleName, {Prim, OneOfs, Calls}) ->
    header(ThisModuleName) ++
	[reuse_fun(ModuleName),
	 erl_syntax:function(
	  erl_syntax:atom(args_for),
	  prim_funcs(ModuleName, ThisModuleName, Prim) ++
	      oneofs_funcs(ModuleName, ThisModuleName, OneOfs) ++
	      call_funcs(ModuleName, ThisModuleName, Calls))].

reuse_fun(Module) ->
    erl_syntax:function(
      erl_syntax:atom(args_for_op),
      [erl_syntax:clause(
	 [erl_syntax:match_expr(
	    erl_syntax:tuple(
	      [erl_syntax:atom(empty),
	       erl_syntax:atom(empty)]),
	    erl_syntax:variable("State")),
	  erl_syntax:variable("Code")],
	 [],
	 [erl_syntax:application(
	    erl_syntax:atom(args_for),
	    [erl_syntax:variable("State"),
	     erl_syntax:variable("Code")])]),
       erl_syntax:clause(
	 [erl_syntax:match_expr(
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
		erl_syntax:atom(Module),
		erl_syntax:atom(get_instances_of_sym)),
	      [erl_syntax:variable("Code"),
	       erl_syntax:variable("SymState"),
	       erl_syntax:variable("RawState")]),
	    [erl_syntax:clause(
	       [erl_syntax:nil()],
	       [],
	       [erl_syntax:application(
		  erl_syntax:atom(args_for),
		  [erl_syntax:variable("State"),
		   erl_syntax:variable("Code")])]),
	     erl_syntax:clause(
	       [erl_syntax:variable("List")],
	       [],
	       [erl_syntax:application(
		  erl_syntax:atom(oneof),
		  [erl_syntax:cons(
		     erl_syntax:application(
		       erl_syntax:atom(args_for),
		       [erl_syntax:variable("State"),
			erl_syntax:variable("Code")]),
		     erl_syntax:variable("List"))])])])])]).

header(Module) ->
  [erl_syntax:attribute(erl_syntax:atom(module),[erl_syntax:atom(Module)]),
   mkinclude("eqc/include/eqc.hrl"),
   erl_syntax:attribute(erl_syntax:atom(compile),[erl_syntax:atom(export_all)])
  ].

mkinclude(String) ->
  erl_syntax:attribute(erl_syntax:atom(include_lib),[erl_syntax:string(String)]).

prim_funcs(ModuleName, _ThisModuleName, Prim) ->
    [erl_syntax:clause([erl_syntax:variable("_State"),
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
    [erl_syntax:clause([erl_syntax:variable(underscore_if_ne("State", PNodes)),
			erl_syntax:abstract(Code)],
		       none,
		       [erl_syntax:application(
			  erl_syntax:atom(oneof),
			  [erl_syntax:list(lists:map(fun calls_for/1, PNodes))])])
     || {oneOf, Code, #diagram_node{http_request = no}, PNodes} <- OneOfs].
call_funcs(ModuleName, _ThisModuleName, Calls) ->
    [erl_syntax:clause([erl_syntax:variable(
			  underscore_if_ne("State", case (This) of
							static -> [];
							Else -> Else end ++ Params)),
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
					        erl_syntax:list(lists:map(fun calls_for/1, Params))])
			  ])])])
     || {acall, Code, #diagram_node{content = Callback}, {This, Params}} <- Calls].

underscore_if_ne(Var, []) -> [$_|Var];
underscore_if_ne(Var, _) -> Var.

this_call(List) when is_list(List) -> calls_for(List);
this_call(Else) -> erl_syntax:abstract(Else).

calls_for(Node) -> erl_syntax:application(
		      erl_syntax:atom(args_for_op),
		      [erl_syntax:variable("State"),
		       erl_syntax:string(Node)]).

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

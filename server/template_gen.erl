%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Generates a PBT model from dependency graphs
%%% @end
%%% Created : 14 Sep 2014 by Pablo Lamela Seijas
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
-module(template_gen).

-include("records.hrl").

-export([fun_templates/3, fun_templates/4, callback_to_map/1]).

fun_templates(Pid, N, Path, ModuleName) ->
    fun_templates(parser_newstruct:get_drai(Pid, N), Path, ModuleName).

fun_templates(Drai, Path, ModuleName) ->
    List = dict:to_list(Drai#drai.dnodes),
    Consts = [{Id, value_to_map(Value)} ||
		 {Id, #diagram_node{content = #value{} = Value}} <- List],
    Calls = [{Id, callback_to_map(Callback)} ||
		{Id, #diagram_node{
			content = (#callback{} = Callback)
		       }} <- List],
    file:write_file(Path ++ atom_to_list(ModuleName) ++ ".erl",
		    [[erl_prettypr:format(AST), "\n"]
		     || AST <- gen_ast(Consts, Calls, ModuleName)]).


    %% [{Id, MethodName, Class, length(Parameters), Parameters} || {Id, #diagram_node{content = #callback{method_name = MethodName, class_signature = Class, params = Parameters}}} <- dict:to_list(Drai#drai.dnodes)].

callback_to_map(#callback{} = OriCallback) ->
    Callback = OriCallback#callback{
		 params = [leave_only_type(Param) ||
			      Param <- OriCallback#callback.params],
		 return = leave_only_type(OriCallback#callback.return),
		 this = leave_only_type(OriCallback#callback.this)},
    lists:foldl(fun maps:remove/2,
		record_to_map(record_info(fields, callback), Callback),
		[depth, kind, tags]).

leave_only_type(#value{type = Type}) -> Type;
leave_only_type(Else) -> Else.

value_to_map(#value{obj_info = ObjInfo} = OriVal) ->
    Val = OriVal#value{obj_info = obj_info_to_map(ObjInfo)},
    record_to_map(record_info(fields, value), Val).

obj_info_to_map(none) -> maps:new();
obj_info_to_map(#obj_info{} = ObjInfo) ->
    record_to_map(record_info(fields, obj_info), ObjInfo).

record_to_map(FieldList, Record) -> 
    IndexedFieldList = lists:zip(lists:seq(2, 1 + (length(FieldList))),
				 FieldList),
    element(2, lists:foldl(fun record_to_map_fold/2, {Record, maps:new()},
			   IndexedFieldList)).

record_to_map_fold({Idx, FieldName}, {RecTuple, Acc}) ->
    {RecTuple, maps:put(FieldName, element(Idx, RecTuple), Acc)}.


gen_ast(Objs, Calls, ModuleName) ->
    [module_declaration(ModuleName),
     export_declaration(),
     constant_function(Objs),
     callback_function(Calls)].

module_declaration(ModuleName) ->
    erl_syntax:attribute(
      erl_syntax:atom(module),
      [erl_syntax:atom(ModuleName)]).

export_declaration() ->
    erl_syntax:attribute(
      erl_syntax:atom(export),
      [erl_syntax:list([export_declaration_item(constant,2),
			export_declaration_item(callback,3)])]).

export_declaration_item(FunctionName,FunctionArity) ->
    erl_syntax:arity_qualifier(
      erl_syntax:atom(FunctionName),
      erl_syntax:integer(FunctionArity)).

constant_function(Objs) ->
    erl_syntax:function(erl_syntax:atom(constant),
			[constant_clause(Id, Map) || {Id, Map} <- Objs]).

constant_clause(Id, Map) ->
    erl_syntax:clause([erl_syntax:abstract(Id),
		       map_abstract(Map)],
		      none,
		      [erl_syntax:abstract({id, Id})]).

map_abstract(Map) when is_map(Map) ->
    erl_syntax:map_expr(
      [erl_syntax:map_field_exact(map_abstract(Key),
				  map_abstract(Value))
        || {Key, Value} <- maps:to_list(Map)]);
map_abstract(Else) -> erl_syntax:abstract(Else).

callback_function(Objs) ->
    erl_syntax:function(erl_syntax:atom(callback),
			[callback_clause(Id, Map) || {Id, Map} <- Objs]).

callback_clause(Id, Map) ->
    erl_syntax:clause([erl_syntax:abstract(Id),
		       map_abstract(Map),
		       erl_syntax:list(get_params(Map))],
		      none,
		      [case maps:get(return, Map) of
			   void -> erl_syntax:atom(void);
			   _ -> erl_syntax:abstract({id, Id})
		       end]).

get_params(Map) ->
    This = maps:get(this, Map),
    ThisInt = case This of
		  static -> 0;
		  new -> 0;
		  undefined -> 0;
		  _ -> 1
	      end,
    Params = maps:get(params, Map),
    ParamList = numerate_params(ThisInt, length(Params)),
    case ThisInt of
	0 -> ParamList;
	1 -> [erl_syntax:variable("_This")|ParamList]
    end.

numerate_params(N, Len) ->
    Nums = lists:seq(1 + N, Len + N),
    List = lists:map(fun (X) -> "_Param" ++ integer_to_list(X) end, Nums),
    lists:map(fun erl_syntax:variable/1, List).

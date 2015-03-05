%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Example of target interface for eqc fsm
%%% @end
%%% Created : 13 Nov 2014 by Pablo Lamela Seijas
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
-module(iface).
-export([callback/3, actual_callback/5, evaluate/2]).

-include_lib("eqc/include/eqc.hrl").

% Callback dummy iface
callback({SymState, RawState}, Code, P) ->
    {SymSuperState, SymSubState} = case SymState of
				       empty -> {1, utils:initial_state_sym()};
				       {N, M} -> {N, M}
				   end,
    {call, iface, evaluate, [Code|?LET(Params, P,
				       begin
					   UpSymState = utils:update_symsubstate(Params, SymSubState),
					   ?LET({CheckParams, UpSymState2}, ?SIZED(Size, utils:add_checks(Size, Code, UpSymState)),
						[{{SymSuperState + 1, UpSymState2}, RawState,
						  [Params|CheckParams]}])
				       end)]}.

evaluate(_, {_SymState, RawState, [Params|Checks]}) ->
    {RawSuperState, RawSubState} = case RawState of
				       empty -> io:format("~n"),
						{1, utils:initial_state_raw()};
				       {N, M} -> {N, M}
				   end,
    [_Result, NewRawSubState|_Inter] = lists:reverse(eqc_symbolic:eval(utils:serialise_trace_with_state(RawSubState, Params))),
    {FinalRawSubState, _} = lists:foldr(fun evaluate_checks/2, {NewRawSubState, 1}, Checks),
    case Checks of
	[] -> ok;
	_ -> io:format("// End of postconditions~n")
    end,
    {RawSuperState + 1, FinalRawSubState}.

evaluate_checks(Param, {RawState, N}) ->
    io:format("// Postcondition: ~p~n", [N]),
    [_Result, NewRawSubState|_Inter] = lists:reverse(eqc_symbolic:eval(utils:serialise_trace_with_state(RawState, Param))),
    {NewRawSubState, N + 1}.


actual_callback(State, Code, WhatToReturn, Info, []) ->
    {NewState, Result} = actual_callback(State, Code, Info, []),
    case WhatToReturn of
	return -> {NewState, Result}
    end;
actual_callback(State, Code, WhatToReturn, Info,  {This, Params}) ->
    {NewState, Result} = actual_callback(State, Code, Info, {This, Params}),
    case WhatToReturn of
	return -> {NewState, Result};
	this -> {NewState, This};
	{param, N} -> {NewState, lists:nth(N, Params)}
    end.
actual_callback(State, Code, #{obj_info := #{},
			       type := Type,
			       value := Value}, []) ->
    Num = utils:get_num_var_raw(State),
    Result = case Type of
		 null -> {jvar, Num, is_null};
		 _ -> {jvar, Num}
	     end,
    io:format("~s ~s = ~p;~n", [type_to_java(Type), name_for({Result, no_cast}), Value]),
    {utils:add_all_params_to_state_raw(Code, State, [Result]), Result};
actual_callback(State, Code, #{class_signature := ClassSignature,
			       method_name := "<init>",
			       method_signature := Signature}, {static, ParamList}) ->
    Result = {jvar, utils:get_num_var_raw(State)},
    io:format("~s ~s = new ~s(~s);~n", [class_to_normal_notation(ClassSignature),
					 name_for({Result, no_cast}),
					 class_to_normal_notation(ClassSignature),
					 mk_param_list(ParamList,Signature)]),
    {utils:add_all_params_to_state_raw(Code, State, [Result, static | ParamList]), Result};
actual_callback(State, Code, #{class_signature := ClassSignature,
			       method_name := Name,
			       method_signature := Signature}, {static, ParamList}) ->
    Result = {jvar, utils:get_num_var_raw(State)},
    Ret = return_from_sig(Signature),
    case Ret of
	"void" -> io:format("~s.~s(~s);~n",
			    [class_to_normal_notation(ClassSignature),
			    Name, mk_param_list(ParamList,Signature)]);
	_ -> io:format("~s ~s = ~s.~s(~s);~n",
		       [Ret,
			name_for({Result, no_cast}),
			class_to_normal_notation(ClassSignature),
			Name, mk_param_list(ParamList,Signature)])
    end,
    {utils:add_all_params_to_state_raw(Code, State, [Result, static | ParamList]), Result};
actual_callback(State, Code, #{method_name := Name,
			       method_signature := Signature}, {This, ParamList}) ->
    Result = {jvar, utils:get_num_var_raw(State)},
    Ret = return_from_sig(Signature),
    case Ret of
	"void" -> io:format("~s.~s(~s);~n", [name_for({This, no_cast}), Name,
					     mk_param_list(ParamList,Signature)]);
	_ -> io:format("~s ~s = ~s.~s(~s);~n",
		       [Ret, name_for({Result, no_cast}), name_for({This, no_cast}), Name,
			mk_param_list(ParamList,Signature)])
    end,
    {utils:add_all_params_to_state_raw(Code, State, [Result, This | ParamList]), Result};
actual_callback(State, Code, Rec, ParamList) ->
    io:format("~nState:~p~nCode:~p~nRec:~p~nParamList:~p~n",
	      [State, Code, Rec, ParamList]),
    Result = {jvar, utils:get_num_var_raw(State)},
    io:format("Result: ~p~n~n", [Result]),
    {utils:add_all_params_to_state_raw(Code, State, [Result | ParamList]), Result}.

mk_param_list(ParamList,Signature) ->
    list_to_commasep_str(
      lists:map(fun name_for/1,
		lists:zip(ParamList,
			  get_param_types(Signature)))).

get_param_types([$(|List]) -> get_param_types(List);
get_param_types([$)|_]) -> [];
get_param_types(List) ->
    case get_param_types_aux(List) of
	{Result, RemList} -> [Result|get_param_types(RemList)]
    end.

get_param_types_aux([$L|Rest]) ->
    {Class, [_|Remaining]} = lists:splitwith(diffrom($;), Rest),
    {class_to_normal_notation([$L|Class] ++ ";"), Remaining};
get_param_types_aux([$[|Rest]) ->
    {_, Remaining} = get_param_types_aux(Rest),
    {null, Remaining}; % We return null because arrays are not implemented
get_param_types_aux([Char|Rest]) ->
    {class_to_normal_notation([Char]), Rest}.


return_from_sig(String) ->
    class_to_normal_notation(tl(lists:dropwhile(diffrom($)), String))).

diffrom(Char) -> fun (T) -> Char =/= T end.

type_to_java(integer) -> "int";
type_to_java(float) -> "float";
type_to_java(double) -> "double";
type_to_java(null) -> "Object";
type_to_java(string) -> "String";
type_to_java(stringBuffer) -> "java.lang.StringBuffer";
type_to_java(stringBuilder) -> "java.lang.StringBuilder";
type_to_java(class) -> "Class<?>";
type_to_java(boolean) -> "boolean";
type_to_java(char) -> "char";
type_to_java(short) -> "short";
type_to_java(long) -> "long".

name_for({this, _}) -> "this";
name_for({{jvar, Num, is_null}, Cast}) when Cast =/= no_cast ->
    "(" ++ Cast ++ ") var" ++ integer_to_list(Num);
name_for({{jvar, Num}, _}) -> "var" ++ integer_to_list(Num);
name_for({{jvar, Num, is_null}, _}) -> "var" ++ integer_to_list(Num).

class_to_normal_notation([]) -> [];
class_to_normal_notation([$B]) -> "byte";
class_to_normal_notation([$C]) -> "char";
class_to_normal_notation([$D]) -> "double";
class_to_normal_notation([$F]) -> "float";
class_to_normal_notation([$I]) -> "int";
class_to_normal_notation([$J]) -> "long";
class_to_normal_notation([$S]) -> "short";
class_to_normal_notation([$Z]) -> "boolean";
class_to_normal_notation([$V]) -> "void";
class_to_normal_notation([$L|Rest]) -> class_to_normal_notation_aux(Rest).
class_to_normal_notation_aux(";") -> [];
class_to_normal_notation_aux([$/|Rest]) -> [$.|class_to_normal_notation_aux(Rest)];
class_to_normal_notation_aux([Char|Rest]) -> [Char|class_to_normal_notation_aux(Rest)].

list_to_commasep_str(L) -> lists:flatten(list_to_commasep_str_aux(L)).
list_to_commasep_str_aux([]) -> "";
list_to_commasep_str_aux([H|[]]) -> [io_lib:format("~s", [H])];
list_to_commasep_str_aux([H|T]) -> [io_lib:format("~s, ", [H])|list_to_commasep_str_aux(T)].

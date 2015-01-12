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
-export([callback/3, actual_callback/4, evaluate/2]).

-include_lib("eqc/include/eqc.hrl").

% Callback dummy iface
callback({SymState, RawState}, Code, P) ->
    {SymSuperState, SymSubState} = case SymState of
				       empty -> {1, utils:initial_state_sym()};
				       {N, M} -> {N, M}
				   end,
    {call, iface, evaluate, [Code|?LET(Params, P, [{{SymSuperState + 1, utils:update_symsubstate(Params, SymSubState)}, RawState, Params}])]}.

evaluate(_, {{_, _}, RawState, Params}) ->
    {RawSuperState, RawSubState} = case RawState of
				       empty -> {1, utils:initial_state_raw()};
				       {N, M} -> {N, M}
				   end,
    [_Result, NewRawSubState|_Inter] = lists:reverse(eqc_symbolic:eval(utils:serialise_trace_with_state(RawSubState, Params))),
    {RawSuperState + 1, NewRawSubState}.

actual_callback(State, Code, #{obj_info := #{},
				value := Value}, []) ->
    Result = {jvar, utils:get_num_var_raw(State)},
    io:format("~p := ~p;~n", [Result, Value]),
    {utils:add_result_to_state_raw(Code, State, Result), Result};
actual_callback(State, Code, #{class_signature := ClassSignature,
			       method_name := "<init>"}, {static, ParamList}) ->
    Result = {jvar, utils:get_num_var_raw(State)},
    io:format("~p := new ~s(~s);~n", [Result,
				     class_to_normal_notation(ClassSignature),
				     list_to_commasep_str(ParamList)]),
    {utils:add_result_to_state_raw(Code, State, Result), Result};
actual_callback(State, Code, #{class_signature := ClassSignature,
				method_name := Name}, {static, ParamList}) ->
    Result = {jvar, utils:get_num_var_raw(State)},
    io:format("~p := ~s.~s(~s);~n", [Result,
				     class_to_normal_notation(ClassSignature),
				     Name,
				     list_to_commasep_str(ParamList)]),
    {utils:add_result_to_state_raw(Code, State, Result), Result};
actual_callback(State, Code, #{method_name := Name}, {This, ParamList}) ->
    Result = {jvar, utils:get_num_var_raw(State)},
    io:format("~p := (~p).~s(~s);~n", [Result, This, Name,
				       list_to_commasep_str(ParamList)]),
    {utils:add_result_to_state_raw(Code, State, Result), Result};
actual_callback(State, Code, Rec, ParamList) ->
    io:format("~nState:~p~nCode:~p~nRec:~p~nParamList:~p~n",
	      [State, Code, Rec, ParamList]),
    Result = {jvar, utils:get_num_var_raw(State)},
    io:format("Result: ~p~n~n", [Result]),
    {utils:add_result_to_state_raw(Code, State, Result), Result}.

class_to_normal_notation([]) -> [];
class_to_normal_notation([$L|Rest]) -> class_to_normal_notation_aux(Rest).
class_to_normal_notation_aux(";") -> [];
class_to_normal_notation_aux([$/|Rest]) -> [$.|class_to_normal_notation_aux(Rest)];
class_to_normal_notation_aux([Char|Rest]) -> [Char|class_to_normal_notation_aux(Rest)].

list_to_commasep_str(L) -> lists:flatten(list_to_commasep_str_aux(L)).
list_to_commasep_str_aux([]) -> "";
list_to_commasep_str_aux([H|[]]) -> [io_lib:format("~p", [H])];
list_to_commasep_str_aux([H|T]) -> [io_lib:format("~p, ", [H])|list_to_commasep_str_aux(T)].

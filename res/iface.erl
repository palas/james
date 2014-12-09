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
-export([callback/3, actual_callback/4]).

-include_lib("eqc/include/eqc.hrl").

% State accessors
initial_state() -> {1, dict:new()}.
add_result_to_state(Code, {N, Dict}, Result) ->
    {N + 1, dict:append(Code, Result, Dict)}.
get_var_num({N, _}) -> N.

% Callback dummy iface
callback(RawState, _Code, Params) ->
    {State, SubState} = case RawState of
			    empty -> io:format("~nNEW TRACE~n"),
				     io:format("=========~n~n"),
				     io:format("STEP 1:~n"),
				     {1, initial_state()};
			    {N, M} -> io:format("~nSTEP ~p:~n", [N]),
				      {N, M}
			end,
    [_Result, NewSubState|_Inter] = lists:reverse(eqc_symbolic:eval(utils:serialise_trace_with_state(SubState, Params))),
    {State + 1, NewSubState}.

actual_callback(State, Code, #{obj_info := #{},
				value := Value}, []) ->
    Result = {jvar, get_var_num(State)},
    io:format("~p := ~p;~n", [Result, Value]),
    {add_result_to_state(Code, State, Result), Result};
actual_callback(State, Code, #{class_signature := ClassSignature,
			       method_name := "<init>"}, {static, ParamList}) ->
    Result = {jvar, get_var_num(State)},
    io:format("~p := new ~s(~s);~n", [Result,
				     class_to_normal_notation(ClassSignature),
				     list_to_commasep_str(ParamList)]),
    {add_result_to_state(Code, State, Result), Result};
actual_callback(State, Code, #{class_signature := ClassSignature,
				method_name := Name}, {static, ParamList}) ->
    Result = {jvar, get_var_num(State)},
    io:format("~p := ~s.~s(~s);~n", [Result,
				     class_to_normal_notation(ClassSignature),
				     Name,
				     list_to_commasep_str(ParamList)]),
    {add_result_to_state(Code, State, Result), Result};
actual_callback(State, Code, #{method_name := Name}, {This, ParamList}) ->
    Result = {jvar, get_var_num(State)},
    io:format("~p := (~p).~s(~s);~n", [Result, This, Name,
				       list_to_commasep_str(ParamList)]),
    {add_result_to_state(Code, State, Result), Result};
actual_callback(State, Code, Rec, ParamList) ->
    io:format("~nState:~p~nCode:~p~nRec:~p~nParamList:~p~n",
	      [State, Code, Rec, ParamList]),
    Result = {jvar, get_var_num(State)},
    io:format("Result: ~p~n~n", [Result]),
    {add_result_to_state(Code, State, Result), Result}.

class_to_normal_notation([]) -> [];
class_to_normal_notation([$L|Rest]) -> class_to_normal_notation_aux(Rest).
class_to_normal_notation_aux(";") -> [];
class_to_normal_notation_aux([$/|Rest]) -> [$.|class_to_normal_notation_aux(Rest)];
class_to_normal_notation_aux([Char|Rest]) -> [Char|class_to_normal_notation_aux(Rest)].

list_to_commasep_str(L) -> lists:flatten(list_to_commasep_str_aux(L)).
list_to_commasep_str_aux([]) -> "";
list_to_commasep_str_aux([H|[]]) -> [io_lib:format("~p", [H])];
list_to_commasep_str_aux([H|T]) -> [io_lib:format("~p, ", [H])|list_to_commasep_str_aux(T)].

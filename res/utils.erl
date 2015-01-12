%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Utils needed by the eqc suites generated
%%% @end
%%% Created : 11 Nov 2014 by Pablo Lamela Seijas
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
-module(utils).

-export([serialise_trace_with_state/2, update_symsubstate/2, initial_state_sym/0,
	 add_result_to_state_sym/2, get_instances_of_sym/3, get_num_var_sym/1,
	 initial_state_raw/0, add_result_to_state_raw/3, get_instance_of_raw/2,
	 get_num_var_raw/1]).


% Symbolic state accessors
initial_state_sym() -> {1, dict:new()}.
add_result_to_state_sym(Code, {N, Dict}) ->
    {N + 1, dict:update(Code, fun (Old) -> [N|Old] end, [N], Dict)}.
get_instances_of_sym(Code, {_N, Dict}, RawState) ->
    case dict:find(Code, Dict) of
	{ok, List} -> [{call, iface, get_instance_of_raw, [Entry, {call, erlang, element, [2, RawState]}]} || Entry <- List];
	error -> []
    end.
get_num_var_sym({N, _}) -> N.
% Raw state accessors
initial_state_raw() -> {1, dict:new()}.
add_result_to_state_raw(_Code, {N, Dict}, Result) ->
    {N + 1, dict:store(N, Result, Dict)}.
get_instance_of_raw(Code, {_N, Dict}) ->
    dict:fetch(Code, Dict).
get_num_var_raw({N, _}) -> N.


serialise_trace_with_state(State, Trace) ->
    {{STrace, _}, {_, _}} = serialise_trace_with_state_aux(Trace, {1, State}),
    STrace.

serialise_trace_with_state_aux({jcall, Mod, Fun, Args}, {AccIn, State}) ->
    {ArgsRes, {InnerAcc, PreState}} = lists:mapfoldl(fun serialise_trace_with_state_aux/2, {AccIn, State}, Args),
    {ReqArgs, SymArgs} = lists:unzip(ArgsRes),
    IA = fun (X) -> InnerAcc + X end,
    IAV = fun (X) -> {var, IA(X)} end,
    {{lists:concat(ReqArgs) ++ [{set, IAV(0), {call, Mod, Fun, [PreState|SymArgs]}},
			        {set, IAV(1), {call, erlang, element, [1, IAV(0)]}},
			        {set, IAV(2), {call, erlang, element, [2, IAV(0)]}}],
      IAV(2)},
     {IA(3), IAV(1)}};
serialise_trace_with_state_aux(Else, Acc) when is_tuple(Else) ->
    {{ReqRes, SymRes}, NewAcc} = serialise_trace_with_state_aux(tuple_to_list(Else), Acc),
    {{ReqRes, list_to_tuple(SymRes)}, NewAcc};
serialise_trace_with_state_aux(Else, Acc) when is_list(Else) ->
    {ElsRes, NewAcc} = lists:mapfoldl(fun serialise_trace_with_state_aux/2, Acc, Else),
    {ReqEls, SymEls} = lists:unzip(ElsRes),
    {{lists:concat(ReqEls), SymEls}, NewAcc};
serialise_trace_with_state_aux(Else, Acc) -> {{[], Else}, Acc}.

update_symsubstate({jcall, _Mod, _Fun, Args}, State) ->
    PreState = lists:foldl(fun update_symsubstate/2, State, Args),
    utils:add_result_to_state_sym(hd(Args), PreState);
update_symsubstate(Else, Acc) when is_tuple(Else) ->
    update_symsubstate(tuple_to_list(Else), Acc);
update_symsubstate(Else, Acc) when is_list(Else) ->
    lists:foldl(fun update_symsubstate/2, Acc, Else);
update_symsubstate(_Else, Acc) -> Acc.

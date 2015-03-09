%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Tests the module trace_gc
%%% @end
%%% Created : 14 Jul 2014 by Pablo Lamela Seijas
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

-module(trace_gc_eqc).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-record(gc_state,{last_idx = 1, available = [], dependencies = [], curr_ds = trace_gc:new_struct()}).

%% @doc Returns the state in which each test case starts. (Unless a different 
%%      initial state is supplied explicitly to, e.g. commands/2.)
-spec initial_state() -> eqc_statem:symbolic_state().
initial_state() ->
    #gc_state{}.

%% ------ Common pre-/post-conditions
%% @doc General command filter, checked before a command is generated.
-spec command_precondition_common(S :: eqc_statem:symbolic_state(),
				  Command :: atom()) -> boolean().
command_precondition_common(_S, _Command) ->
    true.

%% @doc General precondition, applied *before* specialized preconditions.
-spec precondition_common(S :: eqc_statem:symbolic_state(), 
			  C :: eqc_statem:call()) -> boolean().
precondition_common(_S, _Call) ->
    true.

%% @doc General postcondition, applied *after* specialized postconditions.
-spec postcondition_common(S :: eqc_statem:dynamic_state(), 
			   C :: eqc_statem:call(), Res :: term()) -> boolean().
postcondition_common(_S, _Call, _Res) ->
    true.

%% ------ Grouped operator: add_item
%% @doc add_item_command - Command generator
-spec add_item_command(S :: eqc_statem:symbolic_state()) -> 
        eqc_gen:gen(eqc_statem:call()).
add_item_command(S) ->
    case S#gc_state.available of
	[] -> {call, trace_gc, add_item, [S#gc_state.curr_ds, S#gc_state.last_idx, [], false]};
	List -> {call, trace_gc, add_item, [S#gc_state.curr_ds, S#gc_state.last_idx,
					   list(elements(List)), false]}
    end.

%% @doc add_item_pre - Precondition for generation
-spec add_item_pre(S :: eqc_statem:symbolic_state()) -> boolean().
add_item_pre(_S) ->
    true. %% Condition for S

%% @doc add_item_pre - Precondition for add_item
-spec add_item_pre(S :: eqc_statem:symbolic_state(), 
		   Args :: [term()]) -> boolean().
add_item_pre(_S, [_, _Idx, _Deps, _]) ->
    true. %% Condition for S + Args

%% @doc add_item_next - Next state function
-spec add_item_next(S :: eqc_statem:symbolic_state(), 
		    V :: eqc_statem:var(), 
		    Args :: [term()]) -> eqc_statem:symbolic_state().
add_item_next(S, Value, [_, Idx, Deps, _]) ->
    S#gc_state{last_idx = Idx + 1, available = [Idx|S#gc_state.available],
	       dependencies = [{Idx, Dep} || Dep <- Deps]
	       ++ S#gc_state.dependencies, curr_ds = Value}.

%% @doc add_item_post - Postcondition for add_item
-spec add_item_post(S :: eqc_statem:dynamic_state(), 
		    Args :: [term()], R :: term()) -> true | term().
add_item_post(_S, [_, _Idx, _Deps, _], _Res) ->
    true.

%% @doc add_item_blocking - Is the operation blocking in this State 
%% -spec add_item_blocking(S :: eqc_statem:symbolic_state(), 
%%         Args :: [term()]) -> boolean().
%% add_item_blocking(_S, [_, _Idx, _Deps]) ->
%%   false.

%% @doc add_item_adapt - How to adapt a call in this State 
%% -spec add_item_adapt(S :: eqc_statem:symbolic_state(), 
%%     Args :: [term()]) -> boolean().
%% add_item_adapt(_S, [_, _Idx, _Deps]) ->
%%   exit(adapt_not_possible).

%% @doc add_item_dynamicpre - Dynamic precondition for add_item
%% -spec add_item_dynamicpre(S :: eqc_statem:dynamic_state(), 
%%         Args :: [term()]) -> boolean().
%% add_item_dynamicpre(_S, [_, _Idx, _Deps]) ->
%%   true.

%% ------ Grouped operator: remove_item
%% @doc remove_item_command - Command generator
-spec remove_item_command(S :: eqc_statem:symbolic_state()) -> 
        eqc_gen:gen(eqc_statem:call()).
remove_item_command(S) ->
    {call, trace_gc, remove_item, [S#gc_state.curr_ds, elements(S#gc_state.available), false]}.

%% @doc remove_item_pre - Precondition for generation
-spec remove_item_pre(S :: eqc_statem:symbolic_state()) -> boolean().
remove_item_pre(S) ->
    S#gc_state.available =/= [].

%% @doc remove_item_pre - Precondition for remove_item
-spec remove_item_pre(S :: eqc_statem:symbolic_state(), 
		      Args :: [term()]) -> boolean().
remove_item_pre(_S, [_, _Idx, _]) ->
    true. %% Condition for S + Args

%% @doc remove_item_next - Next state function
-spec remove_item_next(S :: eqc_statem:symbolic_state(), 
		    V :: eqc_statem:var(), 
		    Args :: [term()]) -> eqc_statem:symbolic_state().
remove_item_next(S, Value, [_, Idx, _]) ->
    case [1 || {_, Dep} <- S#gc_state.dependencies, Dep =:= Idx] of
	[] -> begin
		  Available = S#gc_state.available -- [Idx],
		  Dependencies = S#gc_state.dependencies,
		  {NewAvailable, NewDependencies} = filter_items(Available, Dependencies),
		  S#gc_state{available = NewAvailable,
			     dependencies = NewDependencies,
			     curr_ds = {call,erlang,element,[1, Value]}}
	      end;
	_ -> S
    end.

%% @doc remove_item_post - Postcondition for remove_item
-spec remove_item_post(S :: eqc_statem:dynamic_state(), 
		       Args :: [term()], R :: term()) -> true | term().
remove_item_post(S, [_, Idx, _], Result) ->
    case {[1 || {_, Dep} <- S#gc_state.dependencies, Dep =:= Idx], Result} of 
	{[], {_NewDS, UnusedIdx}} ->
	    Available = S#gc_state.available -- [Idx],
	    Dependencies = S#gc_state.dependencies,
	    {NewAvailable, _} = filter_items(Available, Dependencies),
	    AvaDelta = Available -- NewAvailable,
	    lists:usort(AvaDelta) =:= lists:usort(UnusedIdx);
	{_, error} -> true;
	_ -> false
    end.

%% @doc remove_item_blocking - Is the operation blocking in this State 
%% -spec remove_item_blocking(S :: eqc_statem:symbolic_state(), 
%%         Args :: [term()]) -> boolean().
%% remove_item_blocking(_S, [_, _Idx]) ->
%%   false.

%% @doc remove_item_adapt - How to adapt a call in this State 
%% -spec remove_item_adapt(S :: eqc_statem:symbolic_state(), 
%%     Args :: [term()]) -> boolean().
%% remove_item_adapt(_S, [_, _Idx]) ->
%%   exit(adapt_not_possible).

%% @doc remove_item_dynamicpre - Dynamic precondition for remove_item
%% -spec remove_item_dynamicpre(S :: eqc_statem:dynamic_state(), 
%%         Args :: [term()]) -> boolean().
%% remove_item_dynamicpre(_S, [_, _Idx]) ->
%%   true.

%% ------ ... more operations

%% @doc <i>Optional callback</i>, Invariant, checked for each visited state 
%%      during test execution.
%% -spec invariant(S :: eqc_statem:dynamic_state()) -> boolean().
invariant(#gc_state{available = Available, curr_ds = DS, dependencies = Deps}) ->
    {RefRecRes, NumRecRes, MinRes, MaxRes, RecNumRes, MinRes2, MaxRes2} = trace_gc:check_residues(DS),
    (MinRes >= 0) andalso (MinRes2 >= 0) andalso (MaxRes =:= MaxRes2) andalso (MaxRes =< length(Deps))
	andalso lists:usort(Available) =:= RefRecRes
	andalso RefRecRes =:= NumRecRes
	andalso NumRecRes =:= RecNumRes.

%% @doc weight/2 - Distribution of calls
-spec weight(S :: eqc_statem:symbolic_state(), Command :: atom()) -> integer().
weight(_S, add_item) -> 1;
weight(_S, remove_item) -> 1;
weight(_S, _Cmd) -> 1.

%% @doc Default generated property
-spec prop_gc() -> eqc:property().
prop_gc() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{H, S, Res} = run_commands(?MODULE,Cmds),
		pretty_commands(?MODULE, Cmds, {H, S, Res},
				Res == ok)
	    end).

filter_items(Available, Dependencies) ->
    case filter_items_once(Available, Dependencies) of
	{Available, Dependencies} -> {Available, Dependencies};
	{A, D} -> filter_items(A, D)
    end.

filter_items_once(Available, Dependencies) ->
    DependedOn = lists:usort([Dep || {_, Dep} <- Dependencies]),
    {A, B} = lists:unzip(Dependencies),
    RemovedItems = lists:usort(A ++ B) -- lists:usort(Available),
    DependenciesWithoutRemoved = lists:filter(fun ({X, Y}) -> not (belongs_to(X, RemovedItems) orelse
								   belongs_to(Y, RemovedItems)) end,
					      Dependencies),
    NewDependedOn = lists:usort([Dep || {_, Dep} <- DependenciesWithoutRemoved]),
    NoLongerDependedOn = (DependedOn -- NewDependedOn),
    {Available -- NoLongerDependedOn, DependenciesWithoutRemoved}.

belongs_to(_, []) -> false;
belongs_to(Sth, [Sth|_]) -> true;
belongs_to(Sth, [_|Rest]) -> belongs_to(Sth, Rest).

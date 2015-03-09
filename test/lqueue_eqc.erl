%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Test Suite for queue with loop marker
%%% @end
%%% Created :  9 Mar 2015 by Pablo Lamela Seijas
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

-module(lqueue_eqc).

-define('TARGET_MODULE', lqueue).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-compile(export_all).

-record(state,{lqueue = undefined, rep = undefined}).

%% @doc Returns the state in which each test case starts. (Unless a different 
%%      initial state is supplied explicitly to, e.g. commands/2.)
-spec initial_state() -> eqc_statem:symbolic_state().
initial_state() ->
    #state{}.

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

%% ------ Grouped operator: new
%% @doc new_command - Command generator
-spec new_command(S :: eqc_statem:symbolic_state()) -> 
        eqc_gen:gen(eqc_statem:call()).
new_command(_S) -> 
    {call, ?TARGET_MODULE, new, []}.

%% @doc new_pre - Precondition for generation
-spec new_pre(S :: eqc_statem:symbolic_state()) -> boolean().
new_pre(#state{rep = undefined}) -> true;
new_pre(_) -> false.


%% @doc new_pre - Precondition for new
-spec new_pre(S :: eqc_statem:symbolic_state(), 
	      Args :: [term()]) -> boolean().
new_pre(_S, []) -> true. %% Condition for S + Args

%% @doc new_next - Next state function
-spec new_next(S :: eqc_statem:symbolic_state(), 
	       V :: eqc_statem:var(), 
	       Args :: [term()]) -> eqc_statem:symbolic_state().
new_next(S, LQueue, []) ->
    S#state{lqueue = LQueue, rep = [token]}.

%% @doc new_post - Postcondition for new
-spec new_post(S :: eqc_statem:dynamic_state(), 
	       Args :: [term()], R :: term()) -> true | term().
new_post(_S, [], _Res) -> true.

%% @doc new_blocking - Is the operation blocking in this State 
%% -spec new_blocking(S :: eqc_statem:symbolic_state(), 
%%         Args :: [term()]) -> boolean().
%% new_blocking(_S, []) ->
%%   false.

%% @doc new_adapt - How to adapt a call in this State 
%% -spec new_adapt(S :: eqc_statem:symbolic_state(), 
%%     Args :: [term()]) -> boolean().
%% new_adapt(_S, []) ->
%%   exit(adapt_not_possible).

%% @doc new_dynamicpre - Dynamic precondition for new
%% -spec new_dynamicpre(S :: eqc_statem:dynamic_state(), 
%%         Args :: [term()]) -> boolean().
%% new_dynamicpre(_S, []) ->
%%   true.

%% ------ Grouped operator: init
%% @doc init_command - Command generator
-spec init_command(S :: eqc_statem:symbolic_state()) -> 
        eqc_gen:gen(eqc_statem:call()).
init_command(_S) -> 
    {call, ?TARGET_MODULE, init, [list(int())]}.

%% @doc init_pre - Precondition for generation
-spec init_pre(S :: eqc_statem:symbolic_state()) -> boolean().
init_pre(#state{rep = undefined}) -> true; %% Condition for S
init_pre(_) -> false.

%% @doc init_pre - Precondition for init
-spec init_pre(S :: eqc_statem:symbolic_state(), 
	       Args :: [term()]) -> boolean().
init_pre(_S, [_List]) ->
    true. %% Condition for S + Args

%% @doc init_next - Next state function
-spec init_next(S :: eqc_statem:symbolic_state(), 
		V :: eqc_statem:var(), 
		Args :: [term()]) -> eqc_statem:symbolic_state().
init_next(S, LQueue, [List]) ->
    S#state{lqueue = LQueue, rep = List ++ [token]}.

%% @doc init_post - Postcondition for init
-spec init_post(S :: eqc_statem:dynamic_state(), 
		Args :: [term()], R :: term()) -> true | term().
init_post(_S, [_List], _Res) ->
    true.

%% @doc init_blocking - Is the operation blocking in this State 
%% -spec init_blocking(S :: eqc_statem:symbolic_state(), 
%%         Args :: [term()]) -> boolean().
%% init_blocking(_S, [List]) ->
%%   false.

%% @doc init_adapt - How to adapt a call in this State 
%% -spec init_adapt(S :: eqc_statem:symbolic_state(), 
%%     Args :: [term()]) -> boolean().
%% init_adapt(_S, [List]) ->
%%   exit(adapt_not_possible).

%% @doc init_dynamicpre - Dynamic precondition for init
%% -spec init_dynamicpre(S :: eqc_statem:dynamic_state(), 
%%         Args :: [term()]) -> boolean().
%% init_dynamicpre(_S, [List]) ->
%%   true.

%% ------ Grouped operator: cons
%% @doc cons_command - Command generator
-spec cons_command(S :: eqc_statem:symbolic_state()) -> 
        eqc_gen:gen(eqc_statem:call()).
cons_command(#state{lqueue = LQueue}) -> 
    {call, ?TARGET_MODULE, cons, [int(), LQueue]}.

%% @doc cons_pre - Precondition for generation
-spec cons_pre(S :: eqc_statem:symbolic_state()) -> boolean().
cons_pre(#state{rep = List}) when is_list(List) ->
    true; %% Condition for S
cons_pre(_) -> false.

%% @doc cons_pre - Precondition for cons
-spec cons_pre(S :: eqc_statem:symbolic_state(), 
	       Args :: [term()]) -> boolean().
cons_pre(_S, [_Elem, _LQueue]) ->
    true. %% Condition for S + Args

%% @doc cons_next - Next state function
-spec cons_next(S :: eqc_statem:symbolic_state(), 
		V :: eqc_statem:var(), 
		Args :: [term()]) -> eqc_statem:symbolic_state().
cons_next(#state{rep = List} = S, NewLQueue, [Elem, _LQueue]) ->
    S#state{lqueue = NewLQueue, rep = List ++ [Elem]}.

%% @doc cons_post - Postcondition for cons
-spec cons_post(S :: eqc_statem:dynamic_state(), 
		Args :: [term()], R :: term()) -> true | term().
cons_post(_S, [_Elem, _LQueue], _Res) ->
    true.

%% @doc cons_blocking - Is the operation blocking in this State 
%% -spec cons_blocking(S :: eqc_statem:symbolic_state(), 
%%         Args :: [term()]) -> boolean().
%% cons_blocking(_S, [Elem, LQueue]) ->
%%   false.

%% @doc cons_adapt - How to adapt a call in this State 
%% -spec cons_adapt(S :: eqc_statem:symbolic_state(), 
%%     Args :: [term()]) -> boolean().
%% cons_adapt(_S, [Elem, LQueue]) ->
%%   exit(adapt_not_possible).

%% @doc cons_dynamicpre - Dynamic precondition for cons
%% -spec cons_dynamicpre(S :: eqc_statem:dynamic_state(), 
%%         Args :: [term()]) -> boolean().
%% cons_dynamicpre(_S, [Elem, LQueue]) ->
%%   true.

%% ------ Grouped operator: pop
%% @doc pop_command - Command generator
-spec pop_command(S :: eqc_statem:symbolic_state()) -> 
        eqc_gen:gen(eqc_statem:call()).
pop_command(#state{lqueue = LQueue}) -> 
    {call, ?TARGET_MODULE, pop, [LQueue]}.

%% @doc pop_pre - Precondition for generation
-spec pop_pre(S :: eqc_statem:symbolic_state()) -> boolean().
pop_pre(#state{rep = List}) when is_list(List) ->
    true; %% Condition for S
pop_pre(_) -> false.

%% @doc pop_pre - Precondition for pop
-spec pop_pre(S :: eqc_statem:symbolic_state(), 
	      Args :: [term()]) -> boolean().
pop_pre(_S, [_LQueue]) ->
    true. %% Condition for S + Args

%% @doc pop_next - Next state function
-spec pop_next(S :: eqc_statem:symbolic_state(), 
	       V :: eqc_statem:var(), 
	       Args :: [term()]) -> eqc_statem:symbolic_state().
pop_next(#state{rep = [token|Rest]} = S, _Result, [_LQueue]) ->
    S#state{rep = [token|Rest]};
pop_next(#state{rep = [_|Rest]} = S, Result, [_LQueue]) ->
    S#state{lqueue = {call, erlang, element, [3, Result]}, rep = Rest}.

%% @doc pop_post - Postcondition for pop
-spec pop_post(S :: eqc_statem:dynamic_state(), 
	       Args :: [term()], R :: term()) -> true | term().
pop_post(#state{rep = [token|_]}, [_LQueue], loop) -> true;
pop_post(#state{rep = [Elem|_]}, [_LQueue], {ok, Elem, _NewLQueue}) -> true.
%pop_post(_, _, _) -> false.

%% @doc pop_blocking - Is the operation blocking in this State 
%% -spec pop_blocking(S :: eqc_statem:symbolic_state(), 
%%         Args :: [term()]) -> boolean().
%% pop_blocking(_S, [LQueue]) ->
%%   false.

%% @doc pop_adapt - How to adapt a call in this State 
%% -spec pop_adapt(S :: eqc_statem:symbolic_state(), 
%%     Args :: [term()]) -> boolean().
%% pop_adapt(_S, [LQueue]) ->
%%   exit(adapt_not_possible).

%% @doc pop_dynamicpre - Dynamic precondition for pop
%% -spec pop_dynamicpre(S :: eqc_statem:dynamic_state(), 
%%         Args :: [term()]) -> boolean().
%% pop_dynamicpre(_S, [LQueue]) ->
%%   true.

%% ------ Grouped operator: reset_loop
%% @doc reset_loop_command - Command generator
-spec reset_loop_command(S :: eqc_statem:symbolic_state()) -> 
        eqc_gen:gen(eqc_statem:call()).
reset_loop_command(#state{lqueue = LQueue}) -> 
    {call, ?TARGET_MODULE, reset_loop, [LQueue]}.

%% @doc reset_loop_pre - Precondition for generation
-spec reset_loop_pre(S :: eqc_statem:symbolic_state()) -> boolean().
reset_loop_pre(#state{rep = List}) when is_list(List) -> true; %% Condition for S
reset_loop_pre(_) -> false.

%% @doc reset_loop_pre - Precondition for reset_loop
-spec reset_loop_pre(S :: eqc_statem:symbolic_state(), 
		     Args :: [term()]) -> boolean().
reset_loop_pre(_S, [_LQueue]) ->
    true. %% Condition for S + Args

%% @doc reset_loop_next - Next state function
-spec reset_loop_next(S :: eqc_statem:symbolic_state(), 
		      V :: eqc_statem:var(), 
		      Args :: [term()]) -> eqc_statem:symbolic_state().
reset_loop_next(#state{rep = List} = S, NewLQueue, [_LQueueParam]) ->
    S#state{lqueue = NewLQueue, rep = (List -- [token]) ++ [token]}.

%% @doc reset_loop_post - Postcondition for reset_loop
-spec reset_loop_post(S :: eqc_statem:dynamic_state(), 
		      Args :: [term()], R :: term()) -> true | term().
reset_loop_post(_S, [_LQueue], _Res) ->
    true.

%% @doc reset_loop_blocking - Is the operation blocking in this State 
%% -spec reset_loop_blocking(S :: eqc_statem:symbolic_state(), 
%%         Args :: [term()]) -> boolean().
%% reset_loop_blocking(_S, [LQueue]) ->
%%   false.

%% @doc reset_loop_adapt - How to adapt a call in this State 
%% -spec reset_loop_adapt(S :: eqc_statem:symbolic_state(), 
%%     Args :: [term()]) -> boolean().
%% reset_loop_adapt(_S, [LQueue]) ->
%%   exit(adapt_not_possible).

%% @doc reset_loop_dynamicpre - Dynamic precondition for reset_loop
%% -spec reset_loop_dynamicpre(S :: eqc_statem:dynamic_state(), 
%%         Args :: [term()]) -> boolean().
%% reset_loop_dynamicpre(_S, [LQueue]) ->
%%   true.



%% @doc <i>Optional callback</i>, Invariant, checked for each visited state 
%%      during test execution.
%% -spec invariant(S :: eqc_statem:dynamic_state()) -> boolean().
%% invariant(_S) ->
%%   true.

%% @doc weight/2 - Distribution of calls
-spec weight(S :: eqc_statem:symbolic_state(), Command :: atom()) -> integer().
weight(_S, new) -> 1;
weight(_S, init) -> 1;
weight(_S, cons) -> 2;
weight(_S, pop) -> 1;
weight(_S, reset_loop) -> 1.
%weight(_S, _Cmd) -> 1.

%% @doc Default generated property
-spec prop_lqueue() -> eqc:property().
prop_lqueue() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{H, S, Res} = run_commands(?MODULE,Cmds),
		pretty_commands(?MODULE, Cmds, {H, S, Res},
				Res == ok)
	    end).

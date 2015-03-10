%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Queue with loop marker
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
-module(lqueue).

-export([new/0, init/1, cons/2, pop/1, reset_loop/1]).

-record(lqueue, {old_elems = [], left_buffer = [],
		 right_buffer = [], new_elems = [],
		 current_elems = sets:new()
		}).

new() -> #lqueue{}.

init(List) ->
    {NewList, Elems} = lists:foldl(fun nub_and_collect/2, {[], sets:new()}, List),
    #lqueue{old_elems = lists:reverse(NewList),
	    current_elems = Elems}.

nub_and_collect(Elem, {List, Set}) ->
    case sets:is_element(Elem, Set) of
	true -> {List, Set};
	false -> {[Elem|List], sets:add_element(Elem, Set)}
    end.

cons(Elem, #lqueue{new_elems = List,
		   current_elems = Elems} = Q) ->
    case sets:is_element(Elem, Elems) of
	false -> Q#lqueue{new_elems = [Elem|List],
			  current_elems = sets:add_element(Elem, Elems)};
	true -> Q
    end.

pop(#lqueue{old_elems = [Elem|Rest],
	    current_elems = Elems} = Q) ->
    {ok, Elem, Q#lqueue{old_elems = Rest,
		        current_elems = sets:del_element(Elem, Elems)}};
pop(#lqueue{old_elems = [], left_buffer = [Elem|Rest]} = Q) ->
    pop(Q#lqueue{old_elems = lists:reverse(Elem),
		left_buffer = Rest});
pop(#lqueue{old_elems = [], left_buffer = [],
	   right_buffer = ([_|_] = RightBuffer)} = Q) ->
    pop(Q#lqueue{left_buffer = lists:reverse(RightBuffer),
		right_buffer = []});
pop(_) -> loop.

reset_loop(#lqueue{new_elems = []} = Q) -> Q;
reset_loop(#lqueue{right_buffer = Buf, new_elems = List} = Q) ->
    Q#lqueue{right_buffer = [List|Buf], new_elems = []}.


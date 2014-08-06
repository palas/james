%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Functions to summarise an integer list
%%% @end
%%% Created : 30 Jun 2014 by Pablo Lamela Seijas
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
-module(integer_list).

%% -include_lib("eqc/include/eqc.hrl").

-compile(export_all).

sort_criteria({integer, X}) -> {X, 1};
sort_criteria({integer_range, X, _}) -> {X, 2}.

collapse_integer_list(List) ->
    collapse_integer_list_aux(utils:usort_using(fun sort_criteria/1, List)).

collapse_integer_list_aux([]) -> [];
collapse_integer_list_aux([{integer, X},{integer, Y}|Rest]) when X + 1 =:= Y ->
    collapse_integer_list_aux([{integer_range, X, Y}|Rest]);
collapse_integer_list_aux([{integer, X},{integer_range, X, Y}|Rest]) ->
    collapse_integer_list_aux([{integer_range, X, Y}|Rest]);
collapse_integer_list_aux([{integer, X},{integer_range, Y, Z}|Rest]) when X + 1 =:= Y ->
    collapse_integer_list_aux([{integer_range, X, Z}|Rest]);
collapse_integer_list_aux([{integer_range, X, Y},{integer, Z}|Rest])
  when X =< Z andalso Z =< Y ->
    collapse_integer_list_aux([{integer_range, X, Y}|Rest]);
collapse_integer_list_aux([{integer_range, X, Y},{integer, Z}|Rest]) when Y + 1 =:= Z ->
    collapse_integer_list_aux([{integer_range, X, Z}|Rest]);
collapse_integer_list_aux([{integer_range, X, Y}, {integer_range, X2, Z}|Rest])
  when (X2 =< Y + 1) andalso (Z >= Y) ->
    collapse_integer_list_aux([{integer_range, X, Z}|Rest]);
collapse_integer_list_aux([{integer_range, X, Y}, {integer_range, X2, Z}|Rest])
  when (X2 =< Y + 1) andalso (Z < Y) ->
    collapse_integer_list([{integer_range, X, Y}|Rest]);
collapse_integer_list_aux([Else|Rest]) ->
    [Else|collapse_integer_list(Rest)].

%% prop_collapse_integer_list_idempotent() ->
%%     ?FORALL(IntegerList, integer_list_gen(),
%% 	    begin
%% 		CollapsedIntegerList = collapse_integer_list(IntegerList),
%% 		CollapsedIntegerList =:= collapse_integer_list(CollapsedIntegerList)
%% 	    end).

%% prop_collapse_integer_list_equivalent() ->
%%     ?FORALL(IntegerList, integer_list_gen(),
%% 	    begin
%% 		CollapsedIntegerList = collapse_integer_list(IntegerList),
%% 		CILToList = to_list(CollapsedIntegerList),
%% 		ILToList = to_list(IntegerList),
%% 		collect([{merges, length([{X, Y} || {integer_range, X, Y} <- CollapsedIntegerList] --
%% 					     [{X, Y} || {integer_range, X, Y} <- IntegerList])}],
%% 			lists:usort(ILToList) =:= lists:sort(CILToList))
%% 	    end).

to_list([]) -> [];
to_list([{integer, X}|Rest]) -> [X|to_list(Rest)];
to_list([{integer_range, X, Z}|Rest]) -> lists:seq(X, Z) ++ to_list(Rest).

%% integer_list_gen() ->
%%     list(oneof([{integer, int()}, ?LET({X, D}, {int(), nat()}, {integer_range, X, X + D + 1})])).

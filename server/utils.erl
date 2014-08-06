%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Low level helper functions
%%% @end
%%% Created : 27 Jun 2014 by Pablo Lamela Seijas
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

-export([mk_dic/2, mk_idx/2, mk_idx/3, expand_set_through_idx/2, separate_by/3,
	 is_element_of_any_set/2, sets_map/2, append_to_dict_set/3, id/1,
	 remove_from_dict_set/3, dic_values/1, group_by/2, sort_using/2, usort_using/2,
	 dict_def_find/3]).

% Creates a Dictionary using the result
% of applying the Fun to the elements
% as key
mk_dic(Fun, List) ->
    dict:from_list([{Fun(X), X} || X <- List]).

% Creates a IndexDictionary from a
% Dictionary, for the property Fun
mk_idx(Fun, Dict) ->
    mk_idx(Fun, dict:new(), Dict).
mk_idx(Fun, Ori, Dict) ->
    dict:fold(mk_indexer(Fun),
	      Ori, Dict).

mk_indexer(Fun) ->
    fun (Key, Value, Dict) ->
	    indexer(Fun, Key, Value, Dict)
    end.
indexer(Fun, Key, Value, Dict) ->
    append_to_dict_set(Fun(Value), Key, Dict).

% Adds an element to a Set in a Dictionary
append_to_dict_set(Key, Value, Dict) ->
    dict:update(Key,
		fun (S) -> sets:add_element(Value, S) end,
		sets:from_list([Value]),
		Dict).

% Remove an element from a Set in a Dictionary
% removes the set if it is empty
remove_from_dict_set(Key, Value, Dict) ->
    case dict:find(Key, Dict) of
	{ok, Set} ->
	    NewSet = sets:del_element(Value, Set),
	    case sets:size(NewSet) of
		0 -> dict:erase(Key, Dict);
		_ -> dict:store(Key, NewSet, Dict)
	    end;
	error -> Dict
    end.

% Expands a set of ids to the other end of an index
expand_set_through_idx(Set, Idx) ->
    sets:fold(fun (El, AccSet) ->
		      expand_set_through_idx_aux(El, AccSet, Idx)
	      end, sets:new(), Set).
expand_set_through_idx_aux(El, AccSet, Idx) ->
    sets:union(dict_fetch_default(sets:new(), El, Idx), AccSet).

% Fetch for dict with default
dict_fetch_default(Default, Key, Dict) ->
    case dict:find(Key, Dict) of
	{ok, Value} -> Value;
	error -> Default
    end.

% Makes a list of lists the same length than
% SetsList, were each sublist contains the
% elements of List whose Fun(Element) is
% in SetsList
separate_by(Fun, SetsList, List) ->
    separate_by_aux(Fun, SetsList, List, [[] || _ <- SetsList]).
separate_by_aux(_Fun, _SetsList, [], Result) -> Result;
separate_by_aux(Fun, SetsList, [H|T], Result) ->
    NewResult = separate_by_aux2(Fun, SetsList, Result, H),
    separate_by_aux(Fun, SetsList, T, NewResult).
separate_by_aux2(Fun, [S|RS], [R|RR], Elem) ->
    case sets:is_element(Fun(Elem), S) of
	true -> [[Elem|R]|RR];
	false -> [R|separate_by_aux2(Fun, RS, RR, Elem)]
    end.

% Checks if the Element is element of any
% of the sets of the SetList
is_element_of_any_set(Element, SetList) ->
    lists:any(fun (X) -> sets:is_element(Element, X) end, SetList).

% Map for sets
sets_map(Fun, Set) ->
    sets:from_list(lists:map(Fun, sets:to_list(Set))).


%% Returns the values of a dictionary as a list
dic_values(Dict) ->
    element(2, lists:unzip(dict:to_list(Dict))).

%% Groups a list into a list of lists, by using the comparison
%% criteria Fun
group_by(_Fun, []) -> [];
group_by(Fun, [H|T]) ->
    {Equals, NotEquals} = lists:partition(fun (X) -> Fun(H, X) end, T),
    [[H|Equals]|group_by(Fun, NotEquals)].

%% Sorts by comparing the result of applying the function Fun to
%% each element. Use sorting function SortingFun.
sort_using_with_fun(Fun, List, SortingFun) ->
    SortingFun(fun (X, Y) -> Fun(X) =< Fun(Y) end, List).

sort_using(Fun, List) ->
    sort_using_with_fun(Fun, List, fun lists:sort/2).

usort_using(Fun, List) ->
    sort_using_with_fun(Fun, List, fun lists:usort/2).

id(X) -> X.

dict_def_find(Def, Key, Dict) ->
    case dict:find(Key, Dict) of
	{ok, Value} -> Value;
	error -> Def
    end.

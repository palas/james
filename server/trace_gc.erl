%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Procedures to decide which traces to remove in real-time
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
-module(trace_gc).

-export([new_struct/0, remove_item/2, add_item/3,
	 get_deps_for_item/2, check_residues/1]).

-record(tgc_struct, {ref_rec_dict = dict:new(), num_rec_dict = dict:new(),
		     rec_num_dict = dict:new()}).

% Indexes are: {tag, N} and {trace, N}
% When adding item, we provide a list of indexes that are dependencies

new_struct() -> #tgc_struct{}.

remove_item(DS, Idx) ->
    try remove_item_aux(DS, Idx)
    catch
	throw:error -> error
    end.

remove_item_aux(#tgc_struct{} = DS, Idx) ->
    PreSetOfUnref = get_num_rec(1, DS),
    {NewDS, Deps} = remove_from_struct(DS, Idx),
    SetOfNewUnref = gb_sets:intersection(PreSetOfUnref, Deps),
    gb_sets:fold(fun remove_items_fold/2, {NewDS, []}, SetOfNewUnref).

get_deps_for_item(#tgc_struct{ref_rec_dict = Dict}, Item) ->
    case dict:find(Item, Dict) of
	{ok, Val} -> {ok, gb_sets:to_list(Val)};
	error -> error
    end.

% Remove Aux
remove_items_fold(Idx, {DS, Deleted}) ->
    {NewDS, NewDeleted} = remove_item_aux(DS, Idx),
    {NewDS, [Idx|NewDeleted ++ Deleted]}.

get_num_rec(N, #tgc_struct{num_rec_dict = Dict}) ->
    case dict:find(N, Dict) of
	{ok, Set} -> Set;
	error -> gb_sets:new()
    end.

set_num_rec(N, Set, #tgc_struct{num_rec_dict = Dict} = DS) ->
    DS#tgc_struct{num_rec_dict = dict:store(N, Set, Dict)}.

remove_from_struct(DS, Idx) ->
    SetDependent = get_set_of_dependent(Idx, DS),
    DS2 = del_set_of_dependent(Idx, DS),
    DS3 = del_from_num_dicts(Idx, DS2),
    {update_references(SetDependent,DS3,-1), SetDependent}.
% END Remove Aux

add_item(DS, Idx, Deps) ->
    SetDeps = gb_sets:from_list(Deps),
    DS2 = set_num_rec(0, gb_sets:add_element(Idx, get_num_rec(0, DS)), DS),
    DS3 = add_to_rec_num(0, Idx, DS2),
    DS4 = add_set_of_dependent(Idx, SetDeps, DS3),
    update_references(SetDeps,DS4,1).

check_residues(#tgc_struct{ref_rec_dict = RefRec,
			   num_rec_dict = NumRec,
			   rec_num_dict = RecNum}) ->
    {RRK, RRV} = lists:unzip([{K, gb_sets:to_list(Val)} || {K, Val} <- dict:to_list(RefRec)]),
    RefRecRes = lists:usort(lists:concat([RRK|RRV])),
    {NRK, NRV} = lists:unzip([{K, gb_sets:to_list(Val)} || {K, Val} <- dict:to_list(NumRec)]),
    NumRecRes = lists:usort(lists:concat(NRV)),
    MinRes = lists:min([0|NRK]),
    MaxRes = lists:max([0|NRK]),
    {RNK, RNV} = lists:unzip([{K, Val} || {K, Val} <- dict:to_list(RecNum)]),
    RecNumRes = lists:usort(RNK),
    MinRes2 = lists:min([0|RNV]),
    MaxRes2 = lists:max([0|RNV]),
    {RefRecRes, NumRecRes, MinRes, MaxRes, RecNumRes, MinRes2, MaxRes2}.

%% Auxiliar
%% ========

del_from_num_dicts(Idx, #tgc_struct{rec_num_dict = RecNum} = DS) ->
    case dict:find(Idx, RecNum) of
	error -> DS;
	{ok, 0 = Num} -> begin
			     NewRecNum = dict:erase(Idx, RecNum),
			     NewDS = set_num_rec(Num, gb_sets:del_element(Idx, get_num_rec(Num, DS)), DS),
			     NewDS#tgc_struct{rec_num_dict = NewRecNum}
			 end;
	{ok, _} -> throw(error)
    end.

get_set_of_dependent(Idx, #tgc_struct{ref_rec_dict = RefRecDict}) ->
    case dict:find(Idx, RefRecDict) of
	{ok, Set} -> Set;
	error -> gb_sets:new()
    end.

del_set_of_dependent(Idx, #tgc_struct{ref_rec_dict = RefRecDict} = DS) ->
    DS#tgc_struct{ref_rec_dict = dict:erase(Idx, RefRecDict)}.
add_set_of_dependent(Idx, Deps, #tgc_struct{ref_rec_dict = RefRecDict} = DS) ->
    DS#tgc_struct{ref_rec_dict = dict:store(Idx, Deps, RefRecDict)}.

update_references(SetDependent, #tgc_struct{num_rec_dict = NumRecDict,
					    rec_num_dict = RecNumDict} = DS,
		    Delta) ->
    {_, NewNumRecDict, NewRecNumDict} = gb_sets:fold(fun (X, Y) -> update_ref_fold(X, Y, Delta) end,
						    {SetDependent, NumRecDict, RecNumDict},
						    rec_to_nums(SetDependent, RecNumDict)),
    DS#tgc_struct{num_rec_dict = NewNumRecDict, rec_num_dict = NewRecNumDict}.

update_ref_fold(Key, {Dependent, NumRecDict, RecNumDict}, Delta) ->
    Value = dict:fetch(Key, NumRecDict),
    PresentDependent = gb_sets:intersection(Value, Dependent),
    {NewNumRecDict, NewRecNumDict} =
	update_ref_both_dicts(Key, Dependent, NumRecDict, RecNumDict,
			      Value, PresentDependent, Delta),
    {gb_sets:subtract(Dependent, PresentDependent), NewNumRecDict, NewRecNumDict}.

update_ref_both_dicts(Key, Dependent, NumRecDict, RecNumDict, Value, PresentDependent,Delta) ->
    NewNumRecDict = update_num_rec_dict(Key,Value,Dependent,PresentDependent,NumRecDict,Delta),
    NewRecNumDict = update_rec_num_dict(Key + Delta, PresentDependent, RecNumDict),
    {NewNumRecDict, NewRecNumDict}.

update_num_rec_dict(Key, Value, Dependent, PresentDependant, NumRecDict, Delta) ->
    RemainingValue = gb_sets:subtract(Value, Dependent),
    NewNumRecDict = case gb_sets:size(RemainingValue) of
			0 -> dict:erase(Key, NumRecDict);
			_ -> dict:store(Key, RemainingValue, NumRecDict)
		    end,
    KeyPlusDelta = Key + Delta,
    case gb_sets:size(PresentDependant) of
	0 -> NewNumRecDict;
	_ -> dict:store(KeyPlusDelta,
	       case dict:find(KeyPlusDelta, NewNumRecDict) of
		   {ok, ValuePlusDelta} -> gb_sets:union(PresentDependant, ValuePlusDelta);
                   error -> PresentDependant
               end, NewNumRecDict)
    end.

update_rec_num_dict(Num, PresentDependent, RecNumDict) ->
    gb_sets:fold(fun (X, D) -> dict:store(X, Num, D) end, RecNumDict, PresentDependent).

add_to_rec_num(Num, Idx, #tgc_struct{rec_num_dict = RecNumDict} = DS) ->
    DS#tgc_struct{rec_num_dict = dict:store(Idx, Num, RecNumDict)}.

rec_to_nums(SetDependent, RecNumDict) ->
    element(1, gb_sets:fold(fun resolve_rec_to_num/2, {gb_sets:new(), RecNumDict}, SetDependent)).

resolve_rec_to_num(Dep, {NumSet, RecNumDict}) ->
    {gb_sets:add_element(dict:fetch(Dep, RecNumDict), NumSet), RecNumDict}.

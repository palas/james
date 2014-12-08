%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Generates a file with the data to check postconditions
%%% with EQC
%%% @end
%%% Created :  5 Dic 2014 by Pablo Lamela Seijas
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
-module(check_fsm_gen).

-include("records.hrl").

-export([gen_checks/3, map_abstract/1, find_checks/1]).


-spec gen_checks(#drai{}, string(), atom()) -> atom(ok) | any().
gen_checks(Drai, Path, ModuleName) ->
  ThisModuleName = atom_to_list(ModuleName) ++ "_check",
  Data = find_checks(Drai),
  file:write_file(Path ++ ThisModuleName ++ ".erl",
    check_file(ModuleName, ThisModuleName, Data)).

find_checks(Drai) ->
  {NodeIds, _} = lists:unzip(eqc_fsm_gen:get_control_nodes(dict:to_list(Drai#drai.dnodes))),
  {NodeSet, Deps} = dia_utils:expand_nodes_within_cluster(Drai, dia_utils:resolve_ids(Drai, NodeIds)),
  Nodes = dia_utils:resolve_ids(Drai, sets:to_list(NodeSet)),
  {Nodes, Deps}.

check_file(ModuleName, ThisModuleName, Data) ->
  SyntaxTree = mk_tree(ModuleName, ThisModuleName, Data),
  lists:flatten([io_lib:format("~s~n~n", [erl_prettypr:format(T)]) || T <- SyntaxTree]).

mk_tree(ModuleName, ThisModuleName, Data) ->
  header(ThisModuleName) ++
  [erl_syntax:function(
    erl_syntax:atom(checks_for),
      call_funcs(ModuleName, ThisModuleName, Data))].

header(Module) ->
  [erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
    mkinclude("eqc/include/eqc.hrl"),
    erl_syntax:attribute(erl_syntax:atom(compile), [erl_syntax:atom(export_all)])
  ].

mkinclude(String) ->
  erl_syntax:attribute(erl_syntax:atom(include_lib), [erl_syntax:string(String)]).

call_funcs(_ModuleName, _ThisModuleName, {Nodes, Deps}) ->
  [erl_syntax:clause([erl_syntax:abstract(NodeId)],
    none,
    [erl_syntax:tuple([erl_syntax:string(NodeId),
      erl_syntax:list(lists:map(fun checks_for/1, sets:to_list(dict:fetch(NodeId, Deps)))
        )])])
    || #diagram_node{id = NodeId} <- Nodes].

checks_for(Node) -> erl_syntax:application(
  erl_syntax:atom(checks_for),
  [erl_syntax:string(Node)]).

-spec map_abstract(any()) -> any().
map_abstract(List) when is_list(List) ->
  case io_lib:printable_list(List) of
    true -> erl_syntax:abstract(List);
    false -> erl_syntax:list(lists:map(fun map_abstract/1, List))
  end;
map_abstract(Map) when is_map(Map) ->
  erl_syntax:map_expr(
    [erl_syntax:map_field_assoc(erl_syntax:abstract(Key),
      map_abstract(Value))
      || {Key, Value} <- maps:to_list(Map)]);
map_abstract(Else) -> erl_syntax:abstract(Else).

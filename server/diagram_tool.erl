%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Procedures to write a diagram to a .dot file
%%% @end
%%% Created : 17 Jun 2014 by Pablo Lamela Seijas
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
-module(diagram_tool).

-export([write_diagram/2, write_diagram/3]).

-include("records.hrl").

write_diagram(Nodes,Arcs) -> write_diagram(Nodes, Arcs, false).

write_diagram(Nodes,Arcs,URL) ->
    ["digraph Example { \n",
     "ordering = \"out\" ;\n",
     "ranksep = 2;\n",
     "ratio = \"auto\" ;\n",
     "label = \"Java Interactions\" ;\n",
     write_clustered_nodes(Nodes,URL),
     write_arcs(arc_sorter:sort_arcs(Arcs),URL),
     "}\n\n"].

write_clustered_nodes(Nodes,URL) ->
    Clusters = utils:group_by(fun compare_clusters/1, Nodes),
    lists:map(fun (X) -> write_node_cluster(X, URL) end, Clusters).

write_node_cluster([DiaNode|_] = List,URL) ->
    case get_cluster(DiaNode) of
	none -> write_nodes(List,URL);
	{cluster, Name, Title} -> [cluster_open(Name, Title),
				   write_nodes(List,URL),
				   cluster_close()]
    end.

cluster_open(Name, Title) ->
    ["subgraph cluster" ++ Name ++ " {\n",
     "label = \"" ++ Title ++ "\";\n"].

cluster_close() -> "}\n".

compare_clusters(A) -> case get_cluster(A) of
			   {cluster, C, _} -> {cluster, C};
			   none -> none
		       end.

get_cluster(#diagram_node{cluster = {cluster, Id, Title}}) -> {cluster, Id, Title};
get_cluster(_) -> none.

write_nodes([],_URL) -> "";
write_nodes([#diagram_node{id = Id, label = Label, properties = OldOpts,
			   is_label_term = IsLabelTerm,
			   tags = Tags, class = Class}|Rest],URL)

                                                              ->
    EscLabel = case IsLabelTerm of
		   true -> parser_utils:print_escaped(Label);
		   false -> parser_utils:escape(Label)
	       end,
    Opts = remove_duplicated_colors(add_class_to_opts(add_tags_to_opts(OldOpts, Tags), Class)),
    URLOption = case {URL, lists:member(diamond, OldOpts)} of
		    {{true, _Path, Module}, false} -> ["URL = \"", atom_to_list(Module), ".html#callback-", Id, "\""];
		    {_, _} -> []
		end,
    [[Id, " [label = \"", EscLabel, "\"",
      comma_if_non_empty(URLOption), URLOption,
      comma_if_non_empty(Opts), write_opts(Opts), "] ;\n"], write_nodes(Rest,URL)].
write_arcs([], _URL) -> "";
write_arcs([#diagram_arc{id_start = From,
			 id_end = To,
			 content = Content,
			 properties = Opts}|Rest],URL)


                                                       ->
    URL_Option = case {URL, Content} of
		     {{true, _PATH, Module}, this} -> ["URL = \"", atom_to_list(Module), "_dep.html#this-", From, "-", To, "\""];
		     {{true, _PATH, Module}, {param, N}} -> ["URL = \"", atom_to_list(Module), "_dep.html#param", integer_to_list(N), "-", From, "-", To, "\""];
		     {_, _} -> []
		 end,
    [[From, " -> ", To, " [", write_opts(
				remove_duplicated_colors([{color, 128, 128, 128},arrow_head|Opts])),
	  comma_if_non_empty(URL_Option), URL_Option,
      "] ;\n"], write_arcs(Rest, URL)].

remove_duplicated_colors(List) ->
    remove_duplicated_colors(none, List).
remove_duplicated_colors(none, []) -> [];
remove_duplicated_colors(Else, []) -> [Else];
remove_duplicated_colors(_, [Color|Rest]) when (element(1, Color) =:= color) ->
    remove_duplicated_colors(Color, Rest);
remove_duplicated_colors(Color, [Sth|Rest]) -> [Sth|remove_duplicated_colors(Color, Rest)].

comma_if_non_empty([]) -> ", ";
comma_if_non_empty(_) -> " ".

add_tags_to_opts(Opts, Tags) ->
    NewOpts = add_tags_to_opts1(Opts, Tags),
    add_tags_to_opts2(NewOpts, Tags).

add_tags_to_opts1(Opts, Tags) ->
    case lists:foldl(fun find_color/2, {false, false, false}, Tags) of
	{true, true, true} -> [{color, 0, 0, 0}|Opts];
	{false, false, false} -> [{color, 128, 128, 128}|Opts];
	{G, B, R} -> [{color, translate_bool_to_col(R),
		       translate_bool_to_col(G),
		       translate_bool_to_col(B)}|Opts]
    end.

add_tags_to_opts2(Opts, Tags) ->
    case lists:foldl(fun find_path_highlight/2, none, Tags) of
	none -> Opts;
	dest -> Opts ++ [{color, 0, 255, 0}];
	ori -> Opts ++ [{color, 255, 0, 0}];
	both -> Opts ++ [{color, 255, 255, 0}]
    end.

add_class_to_opts(Opts, Class) ->
    case Class of
	error -> [{bg_color, 255, 200, 200},filled|Opts];
	_ -> Opts
    end.

translate_bool_to_col(true) -> 127;
translate_bool_to_col(false) -> 0.

find_color(is_before, {_, T, A}) -> {true, T, A};
find_color(is_test, {B, _, A}) -> {B, true, A};
find_color(is_after, {B, T, _}) -> {B, T, true};
find_color(_, {A, B, C}) -> {A, B, C}.

find_path_highlight(dest_path, none) -> dest;
find_path_highlight(ori_path, none) -> ori;
find_path_highlight(ori_path, dest_path) -> both;
find_path_highlight(dest_path, ori_path) -> both;
find_path_highlight(_, T) -> T.

write_opts([]) -> "";
write_opts([solid]) -> "style=solid";
write_opts([dashed]) -> "style=dashed";
write_opts([dotted]) -> "style=dotted";
write_opts([filled]) -> "style=filled";
write_opts([arrow_head]) -> "dir=arrowhead";
write_opts([rectangle]) -> "shape=rectangle";
write_opts([ellipse]) -> "shape=ellipse";
write_opts([diamond]) -> "shape=diamond";
write_opts([thick]) -> "penwidth=4";
write_opts([{label, Label}]) -> "label=\"" ++ parser_utils:print_escaped(Label) ++ "\"";
write_opts([{color, R, G, B}]) ->
    Color = make_hex_color(R, G, B),
    "color=\"" ++ Color ++ "\", fontcolor=\"" ++ Color ++ "\"";
write_opts([{bg_color, R, G, B}]) ->
    Color = make_hex_color(R, G, B),
    "fillcolor=\"" ++ Color ++ "\"";
write_opts([{peripheries, N}]) -> "peripheries=" ++ integer_to_list(N);
write_opts([Else]) -> throw({option_not_defined, Else});
write_opts([Opt|Rest]) ->
    [[write_opts([Opt]), ", "], write_opts(Rest)].

make_hex_color(R, G, B) ->
    "#" ++ make_hex_color_aux(R) ++ make_hex_color_aux(G) ++ make_hex_color_aux(B).
make_hex_color_aux(Dec) ->
    Hex = integer_to_list(Dec, 16),
    case length(Hex) of
	L when (L >= 2) -> lists:nthtail(length(Hex) - 2, Hex);
	1 -> [$0|Hex];
	0 -> "00"
    end.

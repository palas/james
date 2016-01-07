%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Provides utility functions for HTML generation
%%% @end
%%% Created :  7 Jan 2016 by Pablo Lamela Seijas
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
-module(html_utils).

-export([escape_html/1, gen_hash/0, add_hook/2]).

-export([fix_annotations/5]).

escape_html([]) -> [];
escape_html([$&|Rest]) -> [$&, $a, $m, $p, $;|escape_html(Rest)];
escape_html([$<|Rest]) -> [$&, $l, $t, $;|escape_html(Rest)];
escape_html([$>|Rest]) -> [$&, $g, $t, $;|escape_html(Rest)];
escape_html([$"|Rest]) -> [$&, $q, $u, $o, $t, $;|escape_html(Rest)];
escape_html([$'|Rest]) -> [$&, $#, $x, $2, $7, $;|escape_html(Rest)];
escape_html([$/|Rest]) -> [$&, $#, $x, $2, $F, $;|escape_html(Rest)];
escape_html([$\t|Rest]) -> escape_html([$\s, $\s, $\s, $\s, $\s, $\s, $\s, $\s|Rest]);
escape_html([$\s|Rest]) -> [$&, $n, $b, $s, $p, $;|escape_html(Rest)];
escape_html([$\n|Rest]) -> [$<, $b, $r, $\s, $/, $>, $\n|escape_html(Rest)];
escape_html([Else|Rest]) -> [Else|escape_html(Rest)].

gen_hash() -> get_random_string(60, "0123456789abcdef").

get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).

add_hook(Annotate, Hash) ->
    case Annotate of
	true -> [{hook, fun (X, Y, Z) -> create_anchor(X, Y, Z, Hash) end}];
	false -> []
    end.

create_anchor(X, Y, Z, Hash) ->
    Inner = Z(X, Y),
    case [Id2 || {create_anchor, Id2} <- erl_syntax:get_ann(X)] of
	[Id|_] -> prettypr:beside(prettypr:null_text(Hash ++ Id ++ "."),
				  prettypr:beside(Inner, prettypr:null_text(Hash)));
	[] -> Z(X, Y)
    end.


fix_annotations(IOList, false, _, _, _) -> IOList;
fix_annotations(IOList,true,Hash,Title,Preffix) ->
    ["<html><head><title>" ++ Title ++ "</title><style type=\"text/css\">"
                                       ":target { background-color: #aff; }"
                                       "</style></head><body><code>",
     re:replace(escape_html(lists:flatten(IOList)), Hash ++ "([^.]*)\\.(((?!" ++ Hash ++ ").)*)" ++ Hash, "<a name=\"" ++ Preffix ++ "\\1\">\\2</a>", [global, dotall, multiline]),
     "</code></body></html>"].
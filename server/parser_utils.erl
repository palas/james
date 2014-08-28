%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Common procedures used by the diverse parsing techniques
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
-module(parser_utils).

-export([group/1, ungroup/1, read_callbacks/1, print_escaped/1, extract_value/1, escape/1,
	 extract_id/1]).

-include("records.hrl").

group(List) -> [{A, lists:reverse(B)} || {A, B} <- gb_trees:to_list(group(List, gb_trees:empty()))].
group([], Dict) -> Dict;
group([{Pid, Trace}|Tail], Dict) -> group(Tail, custom_gb_tree_append(Pid, Trace, Dict)).

ungroup([]) -> [];
ungroup([{A, List}|Rest]) -> [{A, Elem} || Elem <- List] ++ ungroup(Rest).

custom_gb_tree_append(Key, Value, Tree) ->
    case gb_trees:lookup(Key, Tree) of
	{value, OldValue} -> gb_trees:enter(Key, [Value|OldValue], Tree);
	none -> gb_trees:enter(Key, [Value], Tree)
    end.

read_callbacks([]) -> [];
read_callbacks(["START_FREE_EVENT"|Rest]) ->
    {Value, ["END_FREE_EVENT"|NewRest]} = read_value(Rest),
    [#free_event{tag = Value#value.value}|read_callbacks(NewRest)];
read_callbacks(["START_CALLBACK", TypeT, DepthT, DynamicT, MethodName, ClassSignature,
		IsBefore, IsTest, IsAfter, HttpMethod, HttpURL|Rest]) ->
    Type = case TypeT of
	       "ENTER_METHOD" -> enter_method;
	       "EXIT_METHOD" -> exit_method
	   end,
    Depth = list_to_integer(DepthT),
    IsDynamic = case DynamicT of
		    "dynamic" -> true;
		    "static" -> false
		end,
    {Params, Return, This, ["END_CALLBACK"|NewRest]} = read_params(Rest),
    [#callback{kind = Type, depth = Depth, is_dynamic = IsDynamic, method_name = MethodName,
	       class_signature = ClassSignature, params = Params, return = Return,
	       tags = [is_before || "TRUE" =:= IsBefore] ++
		   [is_test || "TRUE" =:= IsTest] ++
		   [is_after || "TRUE" =:= IsAfter],
	       http_request = case {HttpMethod, HttpURL} of
				  {"", ""} -> no;
				  {"GET", URL} -> {get, URL};
				  {"POST", URL} -> {post, URL}
			      end,
	       this = This}|read_callbacks(NewRest)].

read_params(["START_VARS"|Rest]) ->
    read_params([], undefined, undefined, Rest).
read_params(Params, undefined, undefined, ["PARAM"|Rest]) ->
    {Value, NewRest} = read_value(Rest),
    read_params([Value|Params], undefined, undefined, NewRest);
read_params(Params, undefined, undefined, ["RETURN_VOID"|Rest]) ->
    read_params(Params, void, undefined, Rest);
read_params(Params, undefined, undefined, ["RETURN_VALUE"|Rest]) ->
    {Value, NewRest} = read_value(Rest),
    read_params(Params, Value, undefined, NewRest);
read_params(Params, Return, undefined, ["STATIC"|Rest]) ->
    read_params(Params, Return, static, Rest);
read_params(Params, Return, undefined, ["THIS_NEW"|Rest]) ->
    read_params(Params, Return, new, Rest);
read_params(Params, Return, undefined, ["THIS_VALUE"|Rest]) ->
    {Value, NewRest} = read_value(Rest),
    read_params(Params, Return, Value, NewRest);
read_params(Params, Return, This, ["END_VARS"|Rest]) ->
    {lists:reverse(Params), Return, This, Rest}.

read_value(["LONG_VALUE", LongT|Rest]) ->
    {#value{type = long, value = list_to_integer(LongT)}, Rest};
read_value(["FLOAT_VALUE", FloatT|Rest]) ->
    {#value{type = float, value = list_to_float(FloatT)}, Rest};
read_value(["DOUBLE_VALUE", DoubleT|Rest]) ->
    {#value{type = double, value = list_to_float(DoubleT)}, Rest};
read_value(["OBJECT_NULL"|Rest]) ->
    {#value{type = null, value = null}, Rest};
read_value(["STRING_VALUE", String|Rest]) ->
    {#value{type = string, value = String}, Rest};
read_value(["STRING_BUFFER_NEWID_VALUE", Ref, String|Rest]) ->
    {#value{type = stringBuffer, value = String,
	    obj_info = #obj_info{first_time = true, identifier = list_to_integer(Ref) + 243895605}}, Rest};
read_value(["STRING_BUILDER_NEWID_VALUE", Ref, String|Rest]) ->
    {#value{type = stringBuilder, value = String,
	    obj_info = #obj_info{first_time = true, identifier = list_to_integer(Ref) + 243895605}}, Rest};
read_value(["CLASS_VALUE", String|Rest]) ->
    {#value{type = class, value = String}, Rest};
read_value(["OBJECT_ID_TOSTR", Ref, ToString|Rest]) ->
    {#value{type = object, value = ToString,
	    obj_info = #obj_info{first_time = false, identifier = list_to_integer(Ref) + 243895605}}, Rest};
read_value(["OBJECT_NEWID_TOSTR", Ref, ToString|Rest]) ->
    {#value{type = object, value = ToString,
	    obj_info = #obj_info{first_time = true, identifier = list_to_integer(Ref) + 243895605}}, Rest};
read_value(["BOOLEAN_VALUE", BoolT|Rest]) ->
    {#value{type = boolean, value = case BoolT of
					"TRUE" -> true;
					"FALSE" -> false
				    end},
     Rest};
read_value(["BYTE_VALUE", ByteT|Rest]) ->
    {#value{type = byte, value = list_to_integer(ByteT)}, Rest};
read_value(["CHAR_VALUE", CharT|Rest]) ->
    {#value{type = char, value = list_to_integer(CharT)}, Rest};
read_value(["SHORT_VALUE", ShortT|Rest]) ->
    {#value{type = short, value = list_to_integer(ShortT)}, Rest};
read_value(["INTEGER_VALUE", IntT|Rest]) ->
    {#value{type = integer, value = list_to_integer(IntT)}, Rest}.

print_escaped(Desc) ->
    print_escaped_aux(io_lib:format("~p", [Desc])).
escape(Desc) ->
    print_escaped_aux(Desc).

print_escaped_aux(Desc) ->
    re:replace(print_escaped2_aux(Desc), "\\\"", "\\\\\"", [{return, list}, global]).
print_escaped2_aux(Desc) ->
    re:replace(Desc, "\\\\", "\\\\\\\\", [{return, list}, global]).

extract_value(void) -> void;
extract_value(static) -> static;
extract_value(#value{type = object,
		     value = _,
		     obj_info = #obj_info{first_time = _,identifier = Id}}) ->
    {obj, Id};
extract_value(#callback{return = Result}) -> extract_value(Result);
extract_value(#value{type = Type, value = Value}) ->
    {Type, Value}.

extract_id(void) -> void;
extract_id(static) -> static;
extract_id(#value{type = _,
		     value = _,
		     obj_info = #obj_info{first_time = _,identifier = Id}}) when is_integer(Id) ->
    {obj, Id};
extract_id(#callback{return = Result}) -> extract_value(Result);
extract_id(#value{type = Type, value = Value}) ->
    {Type, Value}.

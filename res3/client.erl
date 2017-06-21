%%%-------------------------------------------------------------------
%%% @author Pablo Lamela Seijas <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela
%%% @doc
%%% Interface for communicating with Java
%%% @end
%%% Created :  3 Oct 2014 by Pablo Lamela
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
-module(client).

-export([syn/0, command/1, method_call/5, store_var/1, new_call/3, var/1,
	 null_var/0, string_const/1, close/0, send/1, recv/0, example1/0,
	 example2/0, example3/0, example4/0, type_class/1, type_byte/0,
	 type_char/0, type_double/0, type_float/0, type_int/0, type_long/0,
	 type_short/0, type_boolean/0]).

syn() ->
    client:send({msg, syn, syn}),
    case client:recv() of
	{msg, syn, syn} -> ok;
	error -> error
    end.

command(Command) ->
    client:send({msg, command, Command}),
    case recv() of
	{msg, result, Result} -> Result;
	error -> error
    end.

method_call(Class, Method, This, Params, ParamTypes) ->
    {cmd, method_call, #{class => Class,
			 method => Method,
			 this => This,
			 params => Params,
			 param_types => ParamTypes}}.

store_var(Value) ->
    {cmd, value_store, #{value => Value}}.

new_call(Class, Params, ParamTypes) ->
    {cmd, new_call, #{class => Class,
                      params => Params,
		      param_types => ParamTypes}}.

type_class(Class) -> {type, object_type, Class}.
type_byte() -> {type, byte_type, byte_type}.
type_char() -> {type, char_type, char_type}.
type_double() -> {type, double_type, double_type}.
type_float() -> {type, float_type, float_type}.
type_int() -> {type, int_type, int_type}.
type_long() -> {type, long_type, long_type}.
type_short() -> {type, short_type, short_type}.
type_boolean() -> {type, boolean_type, boolean_type}.

var(Num) -> {var, var, Num}.

null_var() -> {var, null, null}.
string_const(String) -> {var, string, String}.

close() ->
    client:send({msg, close, close}),
    ok.

send(Msg) ->
    Address = list_to_atom("java_server@" ++ net_adm:localhost()),
    {'mbox', Address} ! Msg.

recv() ->
    receive
	{msg, _, _} = Msg -> Msg
    after 1000 -> error
    end.

example1() ->
    ok = client:syn(),
    {result, ok_method_call, Var1} =
	client:command(
	  client:method_call("java.lang.System", "console",
			     client:null_var(), [], [])),
    client:close(),
    Var1.

example2() ->
    ok = client:syn(),
    Main = "eu.prowessproject.jeb.Main",
    BigInteger = "java.math.BigInteger",
    TBigInteger = type_class(BigInteger),
    {result, ok_method_call, Ten} =
	client:command(
	  client:method_call(Main, "getTen",
			     client:null_var(), [], [])),
    io:format("Ten = ~p~n", [Ten]),
    {result, ok_method_call, One} =
	client:command(
	  client:method_call(Main, "getOne",
			     client:null_var(), [], [])),
    io:format("One = ~p~n", [One]),
    {result, ok_method_call, Eleven} =
	client:command(
	  client:method_call(BigInteger, "add",
			     Ten, [One], [TBigInteger])),
    io:format("Eleven = ~p~n", [Eleven]),
    {result, ok_method_call, Null} =
	client:command(
	  client:method_call(Main, "print",
			     client:null_var(),
			     [Eleven], [TBigInteger])),
    io:format("Null = ~p~n", [Null]),
    io:format("~p~n", [
	client:command(
	  client:method_call("", "add",
			     Eleven, [One], [TBigInteger]))]),
    {result, ok_method_call, Twelve} =
	client:command(
	  client:method_call("", "add",
			     Eleven, [One], [TBigInteger])),
    io:format("Twelve = ~p~n", [Twelve]),
    {result, ok_method_call, Null2} =
	client:command(
	  client:method_call(Main, "print",
			     client:null_var(),
			     [Twelve], [TBigInteger])),
    io:format("Null = ~p~n", [Null2]),
    client:close().

example3() ->
    ok = client:syn(),
    Main = "eu.prowessproject.jeb.Main",
    String = "java.lang.String",
    TString = type_class(String),
    Integer = "java.lang.Integer",
    TInteger = type_class(Integer),
    {result, ok_method_call, Ten} =
	client:command(
	  client:new_call(Integer, [client:string_const("10")], [TString])),
    io:format("Ten = ~p~n", [Ten]),
    {result, ok_method_call, One} =
	client:command(
	  client:new_call(Integer, [client:string_const("1")], [TString])),
    io:format("One = ~p~n", [One]),
    {result, ok_method_call, Eleven} =
	client:command(
	  client:method_call(Main, "add",
			     client:null_var(), [One, Ten], [TInteger, TInteger])),
    io:format("Eleven = ~p~n", [Eleven]),
    {result, ok_method_call, Null} =
	client:command(
	  client:method_call(Main, "print",
			     client:null_var(),
			     [Eleven], [TInteger])),
    io:format("Null = ~p~n", [Null]),
    client:close().

example4() ->
    ok = client:syn(),
    Main = "eu.prowessproject.jeb.Main",
    BigInteger = "java.math.BigInteger",
    TBigInteger = type_class(BigInteger),
    {result, ok_method_call, One} =
	client:command(
	  client:method_call(Main, "getOne",
			     client:null_var(), [], [])),
    {result,error_method_call,error_method_call} =
	client:command(
	  client:method_call(BigInteger, "add",
			     One, [client:null_var()], [TBigInteger])),
    {result,error_method_call,error_method_call} =
	client:command(
	  client:method_call(BigInteger, "add",
			     client:null_var(), [client:null_var()], [TBigInteger])),
    {result, ok_method_call, Null} =
	client:command(
	  client:method_call(Main, "print",
			     client:null_var(),
			     [One], [TBigInteger])),
    io:format("Null = ~p~n", [Null]),
    client:close().

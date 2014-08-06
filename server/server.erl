%%%-------------------------------------------------------------------
%%% @author Pablo Lamela <P.Lamela-Seijas@kent.ac.uk>
%%% @copyright (C) 2014, Pablo Lamela Seijas
%%% @doc
%%% Server that collects the packets sent by the JVMTI Agent
%%% @end
%%% Created :  6 Jun 2014 by Pablo Lamela Seijas
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
-module(server).

-behaviour(gen_server).
-include("records.hrl").

%% API
-export([start/0, get_port/1, get_messages/1, new_message/3,
         new_connection/1, clear_messages/1, stop/1,
	 save_messages/2, load_messages/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {socket, connections = [], calls = [], parsed_calls = empty_parsed(),
		prefix = "/home/palas/Escritorio/autosave/"}).
-record(gc, {traces, last_trace_idx, last_fix_idx, dep_struct}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid, Port} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    case gen_server:start(?MODULE, [], []) of
	{ok, Pid} -> case get_port(Pid) of
			 {ok, Port} -> {ok, Pid, Port};
			 Else -> {error, Else}
		     end;
	Else -> Else
    end.

get_port(Pid) -> gen_server:call(Pid, get_port).
get_messages(Pid) -> gen_server:call(Pid, get_messages).
new_message(Pid, Sender, Pkg) -> gen_server:cast(Pid, {new_message, Sender, Pkg, self()}).
new_connection(Pid) -> gen_server:cast(Pid, new_connection).
clear_messages(Pid) -> gen_server:cast(Pid, clear_messages).
send_closed_connection(Pid, OtherPid) ->
    gen_server:cast(Pid, {send_closed_connection, OtherPid}).
save_messages(Pid, File) -> {ok, Messages} = get_messages(Pid),
			    file:write_file(File, term_to_binary(Messages)).
load_messages(Pid, File) ->
    case file:read_file(File) of
	{ok, Bin} -> Messages = binary_to_term(Bin),
		     gen_server:cast(Pid, {set_messages,
					   lists:reverse(Messages)});
	Error -> Error
    end.

stop(Pid) -> gen_server:call(Pid, terminate).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) -> get_socket().

get_socket() ->
    case gen_tcp:listen(0, [{active, true}, {mode, binary}, {packet, raw}]) of
	{ok, Socket} -> Pid = create_listener(self(), Socket),
			{ok, #state{socket = Socket,
				    connections = [Pid]}};
	{error, Reason} -> {stop, {error, Reason}}
    end.

create_listener(Pid, Socket) ->
    spawn_link(fun () -> listener_loop(Pid, Socket) end).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_port, _From, #state{socket = Socket} = State) ->
    case inet:port(Socket) of
	{ok, Port} -> {reply, {ok, Port}, State};
	Else -> {stop, error, Else, State}
    end;
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};
handle_call(get_messages, _From, #state{calls = Calls, parsed_calls = Parsed} = State) ->
    {Rest, NewParsed} = parse_new_calls(Calls, Parsed),
    {reply, {ok, parsed_to_list(NewParsed)}, State#state{calls = Rest,
							 parsed_calls = NewParsed}};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(new_connection, #state{socket = Socket, connections = Conn} = State) ->
    {noreply, State#state{connections = [create_listener(self(), Socket)|Conn]}};
handle_cast({new_message, Sender, Pkg, Pid},
	    #state{calls = Calls, parsed_calls = Parsed} = State) ->
    {Rest, NewParsed} = case [{Sender, Pkg}|Calls] of
			    List when length(List) > 1000 -> parse_new_calls(List, Parsed);
			    List -> {List, Parsed}
			end,
    Pid ! parsed,
    {noreply, State#state{calls = Rest, parsed_calls = NewParsed}};
handle_cast(clear_messages, #state{} = State) ->
    {noreply, State#state{calls = [], parsed_calls = empty_parsed()}};
handle_cast({send_closed_connection, OtherPid},
	    #state{calls = Calls, parsed_calls = Parsed,
		   prefix = Prefix} = State) ->
    {Rest, NewParsed} = parse_new_calls(Calls, Parsed),
    NewParsedMinus = case Prefix of
	none -> NewParsed;
	_ -> save_calls(OtherPid, NewParsed, Prefix),
	     remove_calls(OtherPid, NewParsed)
    end,
    {noreply, State#state{calls = Rest, parsed_calls = NewParsedMinus}};
handle_cast({set_messages, Messages}, #state{} = State) ->
    {noreply, State#state{calls = [], parsed_calls = parsed_from_list(Messages)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{socket = Socket, connections = [Pending|Connections]} = _State) ->
    exit(Pending, normal),
    lists:map(fun (Con) -> Con ! close end, Connections),
    gen_tcp:close(Socket).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

listener_loop(Pid, Socket) ->
    case gen_tcp:accept(Socket) of
	{ok, NewSocket} -> server:new_connection(Pid), listener_loop(Pid, NewSocket, <<>>, 0);
	Error -> server:new_connection(Pid), exit(Error)
    end.
listener_loop(Pid, Socket, Bin, Num) ->
    receive
	{tcp, _Port, Binary} ->
	    send_complete_pkts(Pid, Socket, <<Bin/binary, Binary/binary>>, Num);
	parsed ->
	    case Num of
		N when N >= 1000 ->
		    gen_tcp:send(Socket, <<"2:ok">>),
		    listener_loop(Pid, Socket, Bin, N - 999);
		N -> listener_loop(Pid, Socket, Bin, N + 1)
	    end;
	close -> unlink(Pid), ok;
	{tcp_closed, _Port} -> send_closed_connection(Pid, self()),
			       unlink(Pid), ok
    end.

send_complete_pkts(Pid, Socket, Binary, N) ->
    case try_to_extract_pkt(Binary) of
	{pkt, Pkg, NewBin} ->
	    server:new_message(Pid, self(), Pkg),
	    send_complete_pkts(Pid, Socket, NewBin, N);
	{no_pkt, NewBin} ->
	    listener_loop(Pid, Socket, NewBin, N)
    end.

try_to_extract_pkt(Bin) ->
    case try_to_extract_pkt_num(Bin, []) of
	{pkt, Pkg, NewBin} -> {pkt, Pkg, NewBin};
	not_enough -> {no_pkt, Bin}
    end.
try_to_extract_pkt_num(<<>>, _) ->  not_enough;
try_to_extract_pkt_num(<<Ch/utf8,Rest/binary>>, Num)
  when Ch >= $0 andalso Ch =< $9 ->
    try_to_extract_pkt_num(Rest, [Ch|Num]);
try_to_extract_pkt_num(<<Ch/utf8,Rest/binary>>, Num)
  when Ch =:= $: ->
    try_to_extract_pkt_text(Rest, list_to_integer(lists:reverse(Num))).
try_to_extract_pkt_text(Bin, Length) ->
    case byte_size(Bin) >= Length of
	true -> {pkt, binary_to_list(binary:part(Bin, {0, Length})),
		 binary:part(Bin, {Length, byte_size(Bin) - Length})};
	false -> not_enough
    end.


%% Parser functions ============================================

empty_parsed() -> dict:new().
parse_new_calls(Calls, Parsed) ->
    GroupedCalls = parser_utils:group(Calls),
    {Rest, NewParsed} = lists:mapfoldl(fun parse_call_list/2, Parsed, GroupedCalls),
    NewCalls = parser_utils:ungroup(Rest),
    {NewCalls, NewParsed}.
parsed_to_list(Parsed) -> [{Pid, element(2, lists:unzip(lists:sort(dict:to_list(Trac))))}
			   || {Pid, #gc{traces = Trac}} <- dict:to_list(Parsed)].
parsed_from_list(List) -> dict:from_list([{Pid, #gc{traces = dict:from_list(add_seq(El))}}
					  || {Pid, El} <- List]).

add_seq(List) -> lists:zip(lists:seq(1, length(List)), List).

%% Internal parser

parse_call_list({Pid, List}, Parsed) ->
    {Parsable, NonParsable} = split_parsable(List),
    NewParsed = append_list_to_ps(Pid, parser_utils:read_callbacks(Parsable), Parsed),
    {{Pid, NonParsable}, NewParsed}.

split_parsable(List) ->
    {NonParsable, Parsable} = lists:splitwith(fun ("END_CALLBACK") -> false; (_) -> true end,
					      List),
    {lists:reverse(Parsable), NonParsable}.

save_calls(Pid, Parsed, Prefix) ->
    case dict:find(Pid, Parsed) of
	{ok, Value} -> MessagesToSave = dict:from_list([{Pid, Value}]),
		       file:write_file(Prefix ++ pid_to_string(Pid),
				       term_to_binary(parsed_to_list(MessagesToSave)));
	error -> ok
    end.

remove_calls(_Pid, Parsed) ->
%   dict:erase(Pid, Parsed).
    Parsed.

pid_to_string(Pid) ->
    tl(lists:reverse(tl(lists:reverse(
			  lists:flatten(io_lib:format("~p", [Pid])))))) ++ ".bin".

append_list_to_ps(Pid, ParsableParsed, ParsedStruct) ->
    Storage = get_storage_for_pid(Pid, ParsedStruct),
    NewStorage = lists:foldl(fun index_trace_in_gc/2, Storage, ParsableParsed),
    set_storage_for_pid(Pid, NewStorage, ParsedStruct).

get_storage_for_pid(Pid, ParsedStruct) ->
    case dict:find(Pid, ParsedStruct) of
	{ok, Value} -> Value;
	error -> #gc{traces = dict:new(), last_trace_idx = 1,
		     last_fix_idx = 1, dep_struct = trace_gc:new_struct()}
    end.

set_storage_for_pid(Pid, NewStorage, ParsedStruct) ->
    dict:store(Pid, NewStorage, ParsedStruct).

%-record(gc, {traces, last_trace_idx, last_fix_id, dep_struct}).
index_trace_in_gc(Trace, GC) ->
    case {trace_type(Trace), is_important_trace(Trace)} of
	{exit, false} -> store_and_index(Trace, GC, false);
	{exit, true} -> store_and_index(Trace, GC, true);
	{enter, true} -> store_and_index(Trace, GC, true);
	{free, _} -> remove_and_gc(Trace, GC);
	_ -> GC
    end.

trace_type(#callback{kind = enter_method}) -> enter;
trace_type(#callback{kind = exit_method}) -> exit;
trace_type(#free_event{}) -> free.

is_important_trace(#callback{depth = -1}) -> false;
is_important_trace(_) -> true.

store_and_index(#callback{params = Params,
			  this = This,
			  return = Return} = Trace,GC,IsPermanent) ->
    Deps = find_deps_for_params(lists:concat(lists:map(fun get_idx/1, [This|Params])), GC),
    {GC2, TraceIdx} = register_trace_with_deps(Trace, Deps, GC),
    GC3 = case IsPermanent of
	      true -> register_fix(TraceIdx, GC2);
	      false -> GC2
	  end,
    GC4 = unregister_if_registered(Return, GC3),
    register_item_with_deps(Return, [TraceIdx], GC4).

find_deps_for_params(List, #gc{dep_struct = DS}) ->
    lists:usort(lists:concat([Value
			      || Item <- List,
				 {ok, Value} <- [trace_gc:get_deps_for_item(DS, Item)]])).

register_trace_with_deps(Trace, Deps, #gc{last_trace_idx = IdxNum,
					  traces = Traces,
					  dep_struct = DS} = GC) ->
    Idx = {trace, IdxNum},
    {GC#gc{traces = dict:store(Idx, Trace, Traces),
	   last_trace_idx = IdxNum + 1,
	   dep_struct = trace_gc:add_item(DS, Idx, Deps)}, Idx}.

register_fix(TraceIdx, #gc{last_fix_idx = IdxNum,
			   dep_struct = DS} = GC) ->
    Idx = {fix, IdxNum},
    GC#gc{last_fix_idx = IdxNum + 1,
	  dep_struct = trace_gc:add_item(DS, Idx, [TraceIdx])}.

register_item_with_deps(#value{obj_info = #obj_info{identifier = Tag}}, Deps,
			#gc{dep_struct = DS} = GC) ->
    Idx = {item, Tag},
    GC#gc{dep_struct = trace_gc:add_item(DS, Idx, Deps)};
register_item_with_deps(_, _, GC) -> GC.

remove_and_gc(#free_event{tag = Tag}, GC) ->
    rem_tag_from_struct({item, Tag}, GC).

get_idx(#value{obj_info = #obj_info{identifier = Idx}}) -> [{item, Idx}];
get_idx(_) -> [].

unregister_if_registered(#value{obj_info = #obj_info{identifier = Idx}}, GC) ->
    rem_tag_from_struct({item, Idx}, GC);
unregister_if_registered(_, GC) -> GC.

rem_tag_from_struct(Tag, #gc{traces = Traces, dep_struct = DS} = GC) ->
    {NewDS, Deps} = trace_gc:remove_item(DS, Tag),
    GC#gc{traces = lists:foldl(fun dict:erase/2, Traces, [Idx || {trace, Idx} <- Deps]),
	  dep_struct = NewDS}.

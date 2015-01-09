-module(test).

-export([test/0]).

test() -> {ok, Pid, Port} = server:start(),
	  {ok, Pwd} = file:get_cwd(),
	  os:putenv("_JAVA_OPTIONS", "-agentpath:" ++ Pwd ++ "/../agent/.libs/libjames.so.0.0.0=" ++ integer_to_list(Port)),
	  execute("mvn -f ../example/freq_server_test/pom.xml test"),
	  parser_newstruct:gen_dia_to_files(Pid, 1, Pwd ++ "/freq-"),
	  parser_newstruct:gen_eqc(Pid, 1, Pwd ++ "/", iface).

execute(Cmd) -> Port = open_port({spawn, Cmd}, [eof, {line, 80}]),
		read(Port).

read(Port) ->
    receive
	{Port, {data, {eol, Line}}} -> io:format("~s~n", [Line]),
				       read(Port);
	{Port, {data, {noeol, Line}}} -> io:format("~s", [Line]),
					 read(Port);
	{Port, eof} -> ok
%    after 120000 -> io:format("."), read(Port)
    end.


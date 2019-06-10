-module(node).
-export([start/0]).

start() ->
	io:format("start was called on ~p~n", [self()]),
	loop().

loop() ->
	io:format("loop was called.~n"),
	receive
		{From, Message} ->
			io:format("~p ~p ~p~n", [From, Message, self()]),
			From ! {ok},
			loop();
		Message ->
			io:format("~p~n", [Message]),
			loop()
	end.

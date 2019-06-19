-module(bcast).
-export([start/1, start/2, add_peers/2, broadcast/2]).

start(Name, Peers) ->
	register(Name, spawn(fun() -> init(Name, Peers) end)),
	ok.

start(Name) ->
	register(Name, spawn(fun() -> init(Name) end)),
	ok.

init(Name, Peers) ->
	InitPeers = add_peers(Peers, [{Name, node()}]),
	broadcast(request_peers, InitPeers),
	[P ! {response_peers, InitPeers, self()} || P <- InitPeers],
	loop(Name, InitPeers).

init(Name) ->
	Peers = add_peers([], [{Name, node()}]),
	loop(Name, Peers).

loop(Name, Peers) ->
	io:fwrite("~p: ~p~n", [Name, Peers]),
	receive
		{request_peers, From} ->
			From ! {response_peers, Peers, self()},
			loop(Name, Peers);
		{response_peers, ResponsePeers, From} ->
			UniqPeers = add_peers(Peers, ResponsePeers),
			loop(Name, UniqPeers)
	end.

add_peers(Peers, NewPeers) ->
	UniqPeers = sets:from_list(Peers ++ NewPeers),
	sets:to_list(UniqPeers).

broadcast(request_peers, Peers) ->
	[P ! {request_peers, self()} || P <- Peers],
	ok.

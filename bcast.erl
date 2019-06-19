-module(bcast).
-export([start/1, start/2, add_peers/2]).

start(Name, Peers) ->
	register(Name, spawn(fun() -> loop(Name, init(Peers)) end)),
	{self(), Name}.

start(Name) ->
	register(Name, spawn(fun() -> loop(Name, init([])) end)),
	{self(), Name}.

init(Peers) ->
	Peers.

loop(Name, Peers) ->
	receive
		Any ->
			io:format("Answer is ~p~n", [Any]),
			loop(Name, Peers)
	end.

add_peers(Peers, [NewPeer|T]) ->
	UpdatedPeers = sets:add_element(NewPeer, Peers),
	add_peers(UpdatedPeers, T);
add_peers(Peers, []) ->
	Peers.


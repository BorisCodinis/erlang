-module(blockchain_server).
-export([init/0, mine_block/1, get_blocks/0]).
-import(server1, [rpc/2]).

%% Client routines
mine_block(Data) -> rpc(blockchain_server, {mine_block, Data}).
get_blocks()     -> rpc(blockchain_server, {get_blocks}).

%% Callback routines
init() -> [get_genesis_block()].
handle({mine_block, Data}, Blockchain) -> io:format("Not implemented.~n").
handle(get_blocks) -> io:format("not implemented.~n").

%% Function implementations
get_genesis_block() -> {0, "genesis block", 012876563735}.


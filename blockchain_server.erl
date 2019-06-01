-module(blockchain_server).
-export([init/0, mine_block/1, get_blocks/0]).
-import(server1, [rpc/2]).

%% Client routines
mine_block(Data) -> rpc(blockchain_server, {mine_block, Data}).
get_blocks() -> rpc(get_blocks, {get_blocks}).

%% Callback routines
init() -> [get_genesis_block()].
get_genesis_block() -> {0, "genesis block", 012876563735}.

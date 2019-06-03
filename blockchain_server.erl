-module(blockchain_server).
-export([init/0, mine_block/1, get_blocks/0, handle/2]).
-import(server1, [rpc/2]).
-import(hash, [get_hash/1]).

%% Client routines
mine_block(Data) -> rpc(blockchain_server, {mine_block, Data}).
get_blocks()     -> rpc(blockchain_server, {get_blocks}).

%% Callback routines
init() -> [get_genesis_block()].
handle({mine_block, Data}, Blockchain) -> {ok, Blockchain ++ [create_next_block(Data)]};
handle({get_blocks}, Blockchain) -> {{ok, Blockchain}, Blockchain}.

%% Function implementations
get_genesis_block() -> {0, "genesis block", 012876563735}.
create_next_block(Data) -> {1, Data, hash:get_hash(Data)}.

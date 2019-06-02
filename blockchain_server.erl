-module(blockchain_server).
-export([init/0, mine_block/1, get_blocks/0, handle/2]).
-import(server1, [rpc/2]).

%%
%% Ein Block der Blockchain wird dargestellt als Tupel der Form:
%%	{
%%		index,
%%		previousHash, // Für den ersten Block wird der previousHash als "0" definiert.
%%		timestamp,
%%		data,
%%		hash
%%	}
%%


%% Client routines
mine_block(Data) -> rpc(blockchain_server, {mine_block, Data}).
get_blocks()     -> rpc(blockchain_server, {get_blocks}).

%% Callback routines
init() -> [get_genesis_block()].
handle({mine_block, Data}, Blockchain) -> {ok, Blockchain ++ [create_next_block(Data)]};
handle({get_blocks}, Blockchain) -> {{ok, Blockchain}, Blockchain}.

%% Function implementations
get_genesis_block() -> {0, "0", os:timestamp(), "genesis block", "01287656373543547347346"}.
create_next_block(Data) -> {1, Data, 8352936489}.


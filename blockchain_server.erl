-module(blockchain_server).
-export([init/0, mine_block/1, get_blocks/0, handle/2]).
-import(server1, [rpc/2]).
-import(hash, [get_hash/1]).

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
handle({mine_block, Data}, Blockchain) -> {ok, Blockchain ++ generate_next_block(Data, lists:last(Blockchain))};
handle({get_blocks}, Blockchain) -> {{ok, Blockchain}, Blockchain}.

%% Function implementations
get_genesis_block() ->
	TS = os:timestamp(),
	{0, "0", TS, "genesis block", calculate_hash_for_block(0, "0", TS, "genesis block")}.

%% Erzeuge den nächsten Block.
%% TODO: Neuen Block auf Validität prüfen

generate_next_block(Data, LastBlock) ->
	{Index, _, _, _, Hash} = LastBlock,
	NextIndex = Index + 1,
	NextPreviousHash = Hash,
	NextTimeStamp = os:timestamp(),
	NextData = Data,
	NextHash = calculate_hash_for_block(NextIndex, Hash, NextTimeStamp, Data), %% calculate_hash_for_block/1 muss noch implementiert werden
	[{NextIndex, NextPreviousHash, NextTimeStamp, NextData, NextHash}].

calculate_hash_for_block(Index, PrevHash, Timestamp, Data) ->

	BlockToBeHashed = integer_to_list(Index) ++ PrevHash ++ integer_to_list(element(1, Timestamp)) ++ integer_to_list(element(2, Timestamp)) ++ integer_to_list(element(3, Timestamp)) ++ Data,
	hash:get_hash(BlockToBeHashed).

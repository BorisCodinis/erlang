-module(blockchain_server).
-export([init/0, mine_block/1, get_blocks/0, handle/2, is_valid_chain/1]).
-import(server4, [rpc/2]).
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

%%
%% Client routines
mine_block(Data) -> rpc(blockchain_server, {mine_block, Data}).
get_blocks()     -> rpc(blockchain_server, {get_blocks}).

%%
%% Callback routines
init() -> [get_genesis_block()].

%% TODO: Implement broadcast newly created blockchain to other nodes.
%% TODO: Implement handle broadcast messages
%% ISSUE: ++ List operator is very inefficient as it copies the whole list every time it is used
handle({mine_block, Data}, Blockchain) -> {ok, Blockchain ++ generate_next_block(Data, lists:last(Blockchain))};
handle({get_blocks}, Blockchain) 	   -> {{ok, Blockchain}, Blockchain}.

%%
%% Function implementations
get_genesis_block() ->
	Index = 0,
	PreviousHash = "0",
	TimeStamp = {0000,000000,000000}, %% timestamp must be hardcoded for is_valid_chain to work.
	Data = "genesis block",
	Hash = calculate_hash_for_block(Index, PreviousHash, TimeStamp, Data),

	%% The generated Block
	{Index, PreviousHash, TimeStamp, Data, Hash}.

%% Erzeuge den nächsten Block.
generate_next_block(Data, LastBlock) ->
	{Index, _, _, _, Hash} = LastBlock,
	NextIndex = Index + 1,
	NextPreviousHash = Hash,
	NextTimeStamp = os:timestamp(),
	NextData = Data,
	NextHash = calculate_hash_for_block(NextIndex, Hash, NextTimeStamp, Data),
	NewBlock = {NextIndex, NextPreviousHash, NextTimeStamp, NextData, NextHash},
	
	%% Check whether new Block is valid
	true = is_valid_new_block(NewBlock, LastBlock),

	%% If all went well, return new block.
	[NewBlock].

calculate_hash_for_block(Index, PrevHash, Timestamp, Data) ->
	BlockToBeHashed = 
		   integer_to_list(Index) 
		++ PrevHash 
		++ integer_to_list(element(1, Timestamp)) 
		++ integer_to_list(element(2, Timestamp)) 
		++ integer_to_list(element(3, Timestamp)) 
		++ Data,
	hash:get_hash(BlockToBeHashed).

%% Check whether a block is valid, e.g. matches the previous block
%% All checks are done using pattern matching, so if a match fails, the whole function fails.
is_valid_new_block(NewBlock, PreviousBlock) ->

	%% Extract fields from Blocks
	{NewIndex, NewPreviousHash, NewTimeStamp, NewData, NewHash} = NewBlock,
	{PreviousIndex, _, _, _, PreviousHash} = PreviousBlock,
	
	%% Check index
	NewIndex = (PreviousIndex + 1),

	%% Check previous hash for new block
	NewPreviousHash = PreviousHash,

	%% Check new Hash
	CorrectNewHash = calculate_hash_for_block(NewIndex, NewPreviousHash, NewTimeStamp, NewData),
	NewHash = CorrectNewHash,

	%% If all went well, return true.
	%% TODO: Is this ideomatic erlang?
	true. 
	
%% Check whether a whole blockchain is valid
is_valid_chain(Blockchain) ->
	[GenesisBlock|_] = Blockchain,
	true = is_valid_genesis_block(GenesisBlock),
	true = is_continuous_chain(Blockchain).
	
is_valid_genesis_block(Block) ->
	Block = get_genesis_block(),
	true.

is_continuous_chain([FirstBlock, SecondBlock|T]) ->
	true = is_valid_new_block(SecondBlock, FirstBlock),
	is_continuous_chain([SecondBlock|T]);
is_continuous_chain([_SingleBlock]) ->
	true.
	











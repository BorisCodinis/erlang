-module(hash).
-export([get_hash/1]).

get_hash(Input) ->
	crypto:start(),
	<<HashBin:256/big-unsigned-integer>> = crypto:hash(sha256,Input),
	lists:flatten(io_lib:format("~64.16.0b", [HashBin])).

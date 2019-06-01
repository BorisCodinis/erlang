-module(tut).
-export([double/1,conds/1]).
-compile([debug_info, export_all]).
double(X) ->
	2*X.

conds(Q) ->
	if
		Q == 5 ->
			io:format("zahl: ~w~n", [Q]),
			equals_five;
		Q == 6 ->	
			io:format("zahl: ~w~n", [Q]),
			equals_six;
		true ->
			equals_non
	end.

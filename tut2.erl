-module(tut2).
-export([convert/2]).

convert(N, zentimeter) ->
	N * 2.54;

convert(M, zoll) ->
	M / 2.54.

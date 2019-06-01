-module(tut1).
-export([fak/1, mult/2]).

fak(1) ->
	1;

fak(0) ->
	1;

fak(N) ->
	N * fak(N-1).

mult(X, Y) ->
	X * Y.

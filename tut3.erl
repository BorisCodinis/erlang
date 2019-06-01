-module(tut3).
-export([convert_lenght/1]).

convert_lenght({cm, X}) ->
	{inch, X / 2.54};

convert_lenght({inch, Y}) ->
	{cm, Y * 2.54}.

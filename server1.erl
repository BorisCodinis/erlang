-module(server1).
-export([start/2, rpc/2]).

%% Funktion start, akzeptiert zwei Argumente
%% Name: Name des zu startenden Serverprozesses (???)
%% Mod: Callbackhandler (???)
start(Name, Mod) ->

	%% Registriere einen neuen Prozess unter dem Namen 'Name'.
	%% Diesere Prozess führt die Funktion loop() aus.
	register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

%% Wird von callback handler aufgerufen (tbd), warum??
rpc(Name, Request) ->
	Name ! {self(), Request},
		receive
			{Name, Response} -> Response
		end.

%% Main Server loop
loop(Name, Mod, State) ->
	receive
		{From, Request} ->
			
			%% Verarbeite den Request mit der Methode handle() von Mod
			{Response, State1} = Mod:handle(Request, State),

			%% Sende Response zurück zum Client
			From ! {Name, Response},
			
			%% Weiter loopen (tail recursion!)
			loop(Name, Mod, State1)
	end.

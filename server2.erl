-module(server2).
-export([start/2, rpc/2]).

%%
%% DEPRECATED
%%

%% Funktion start, akzeptiert zwei Argumente
%% Name: Name des zu startenden Serverprozesses (???)
%% Mod: Callbackhandler (???)
start(Name, Mod) ->

	%% Registriere einen neuen Prozess unter dem Namen 'Name'.
	%% Diesere Prozess führt die Funktion loop() aus.
	register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

%% Wird von callback handler aufgerufen
rpc(Name, Request) ->
	Name ! {self(), Request},
		receive
			{Name, crash} -> exit(rpc);
			{Name, ok, Response} -> Response
		end.

%% Main Server loop, wird durch Aufruf von start() als eigener Prozess gestartet.
loop(Name, Mod, OldState) ->
	receive
		{From, Request} ->	

			%% Try to make the module handle the request
			try Mod:handle(Request, OldState) of
				{Response, NewState} ->
					From ! {Name, ok, Response},
					loop(Name, Mod, NewState)
			catch
				_:Why ->
					log_the_error(Name, Request, Why),
					
					%% Send a message to crash the client
					From ! {Name, crash},

					%% Loop on with the original state
					loop(Name, Mod, OldState)
			end
	end.

log_the_error(Name, Request, Why) ->
	io:format("Server ~p request ~p ~n caused exception ~p ~n", [Name, Request, Why]).

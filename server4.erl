
%%
%% This module represents a peer (as in naivechain).
%%


-module(server4).
-export([start/2, rpc/2, swap_code/2]).

%% Funktion start, akzeptiert zwei Argumente
%% Name: Name des zu startenden Serverprozesses (???)
%% Mod: Callbackhandler (???)
start(Name, Mod) ->

	%% Registriere einen neuen Prozess unter dem Namen 'Name'.
	%% Diesere Prozess führt die Funktion loop() aus.
	register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).

%% Swap callback module
%% Works only if callbackmodules can handle the State (e. g. name_server cannont handle blockchain)
%% and the names given to rpc are the same atoms.
swap_code(Name, Mod) -> rpc(Name, {swap_code, Mod}).

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
		
		%% Hot Swap Mod (and thereby the available client routines and callbacks).
		{From, {swap_code, NewCallbackMod}} ->
			From ! {Name, ok, ack},
			
			%% Code swapping is done by just looping on with new callback module.
			loop(Name, NewCallbackMod, OldState);
		
		%% Try Handle request using Mod's handle function
		{From, Request} ->	
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

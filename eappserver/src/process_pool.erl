-module(process_pool).
-export([start/0, stop/0]).
start() ->
	%%initiates a table of pids of generic processes

	{ok, started}.
stop() ->
	{ok, stopped}.
create_process(Flag) ->
	
	{ok, pid}.

execute(Mod, Fun, Args) ->
	%%create process 
	{ok,result}.

get_free_process() ->
	{ok, pid}.

mark_busy(Pid) ->
	%%mark the process with "Pid" as busy in the process table
	{ok, done}.

loop(State) ->
	%receive
		
	%end,
	loop(State).

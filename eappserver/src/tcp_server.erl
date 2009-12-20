%% Author: kiran
%% Created: Aug 1, 2009
%% Description: TODO: Add description to ps_tcp_server
-module(tcp_server).
%%
%% Include files
%%
-include("common.hrl").
%%
%% Exported Functions
%%
-export([start/0]).

%%
%% API Functions
%%

%%
%% TODO: Add description of start/function_arity
%%
start() ->
	start({80, [binary, {packet, 0}, {reuseaddr, true},{active, false}, {backlog, 30}]}).
start({Port, SocketOpts}) -> 
	case gen_tcp:listen(Port, SocketOpts) of
	{ok, ListenSocket} -> 
		io:format("listening to port~p with ~w~n", [Port, {SocketOpts}]),
		mnesia:start(), io:format("mnesia started..~n"),
		%%try spawining for accepting con
		spawn(fun() ->accept_connection(ListenSocket, 0) end);
	Error ->
		io:format("Unable to listen to port~p with ~w~n", [Port, {SocketOpts}])
	end. 
%%
%% Local Functions
%%
accept_connection(ListenSocket, N) ->
	case gen_tcp:accept(ListenSocket) of
	{ok , Socket} ->
		spawn(fun() -> accept_connection(ListenSocket, N+1) end),
		{ok, Pid} = ps_socket:start(Socket),
		gen_tcp:controlling_process(Socket, Pid),
		gen_server:cast(Pid, {recv, tcp});
		%end;
	Error ->
		io:format("<<< ~p  >>>~n", [{Error}])
	end.


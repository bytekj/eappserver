%% Author: kiran khaladkar
%% Created: Aug 6, 2009
%% Description: Start or stop the server.
-module(server).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/1]).

%%
%% API Functions
%%

%%
%% TODO: Add description of start/function_arity
%%
start(tcp) ->
	ps_tcp_server:start(),
	{ok, started};
start(http) ->
	ps_web_server:start(),
	{ok, started}.


%%
%% Local Functions
%%


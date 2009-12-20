%%%-------------------------------------------------------------------------
%%% Author: Kiran Khaladkar
%%% Description: This module manages all the in-memory apps.
%%%		It initiates the apps. Sends a message to a app
%%%		Allow a app to send a message to another app
%%% Created: Sat 5th Dec 2009
%%%-------------------------------------------------------------------------
-module(app_manager).
-compile(export_all).
-define(APP_TABLE, app_table).
-export([init/0, init_app/2, stop_app/2, kill_app/2, request_app/3]).

%%%-------------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------------

%% Function: init/0
%% Description: initializes the app_manager
%% Return: {reply, Reply}
init() ->
	%%SessionPid is the key for the app table
	Reply = started,
	{reply, Reply}.


%% Function: init_app/2
%% Description: initializes an appication as requested by the client.
%% Return: {AppName, AppPid}
init_app(SessionPid, AppName) ->
	{ok, Pid} = gen_app:start(AppName, Param, Options),
	append_to_table(UserName, SessionPid, AppName, Pid),
	{AppName, Pid}.

%% Function: stop_app/2
%% Description: stops an application process gracefully.
%% Return: {ok, Reply}
stop_app(SessionPid, AppName) ->
	%%find app_pid from the app_table and call terminate for that app
	{ok, Reply}.

%% Function: kill_app/2
%% Description: kills an application abruptly
%% Rerturn: {ok, Reply}
kill_app(SessionPid, AppName) ->
	%%Find the app_pid from the app_table and kill the app process
	{ok, killed}.

%% Function: request_app/3
%% Description: sends the request to app corrosponding the session
%%		This request is asynchronous.
%% Return: {ok, Reply} depending upon what app returns.	
request_app(SessionPid, AppName, Req) ->
	%%Find application pid using application name and session pid
	AppId = get_app_id(SessionPid, AppName),
	%%and then send the request to that application.
	Reply=gen_app:handle_async_request(AppId, Req),
	{ok, Reply}.

%%%-------------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------------

%% Function: get_app_id/2
%% Description: Retrieves the application pid from process table of that
%%		client process.
%% Return: {reply, Reply} where Reply is the pid of the application process.
get_app_id(SessionPid, AppName) ->
	Reply = erlang:get(AppName),
	{reply, Reply}.

%% Function: append_to_table/4
%% Description: stores the tuple {AppName, AppPid} in process table of the client process
%%		as this function is called from the client process.
%%		UserName, SessionPid parameters are for future use.
%% Return: Returns the key of the {key, value} stored in the table.
append_to_table(UserName, SessionPid, AppName, AppPid) ->
	Reply=erlang:put(AppName, AppPid),
	{reply, Reply}.

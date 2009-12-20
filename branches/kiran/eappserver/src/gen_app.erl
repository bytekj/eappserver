%%%-------------------------------------------------------------------------
%%% Author: Kiran Khaladkar
%%% Description: Behavior for writing applications.
%%% Created: Sat 5th Dec 2009
%%%-------------------------------------------------------------------------

-module(gen_app).
-export([behavior_info/1, start/3, start/2]).
-behavior(gen_server).

-record(gen_app_state, {app_name :: atom(), 		%%stores app name
			username :: string(), 		%%stores the username of the user
			app_cookie :: string(), 	%%appication specific cookie to be sent to the client
			cookie :: string(), 		%%generic session cookie
			app_state :: tuple() 		%%state maintained by the apps extending this behavior
	       		}).

behavior_info(callbacks) ->
	[{init,1}, {handle_request, 2}].

%% Function: start/3
%% Description: starts the application by AppName with Params and Options.
%%		Options are gen_app process options
%% Return: {ok, Pid}
start(AppName, Param, Options) ->
	{ok, AppInitState, InitResponse}=AppName:init(Param),
	gen_server:start_link(?MODULE, AppInitState, Options).

%% Function: start/2
%% Description: Starts the application by AppName with params but without Options
%% Return: {ok, Pid}
start(AppName, Param) ->
	{ok, AppInitState} = AppName:init(Param),
	gen_server:start_link(?MODULE, AppInitState).

%% Function: get_info/1
%% Return: returns state of the gen_app 
get_info(AppId) ->
	gen_app:handle_sync_request(Appid, get_state).

%% Funtion: handle_async_request(/2
%% Description: API to handle asynchronous calls that return immidiately without results.
%% Rerurn: {noreply, State}
handle_async_request(AppId, Reqest) ->
	gen_server:handle_cast(AppId, {app_req, Reqest}),
	{ok, processing}.

%% Function: handle_sync_request/2
%% Description: API to handle synchronous calls that return the result of the
%%		request after completion of the processing of that request
%% Return: {reply, Reply, NewState} after completion of the processing
handle_sync_request(AppId, Request) ->
	{ok, Response} = gen_server:handle_call(AppId, {app_req, Request}).

%% Funtion: handle_cast/2
%% Desciption: handle cast messages to gen_app process
%% Return: {noreply, #state{}}
handle_cast({app_req, Request}, State = #state{app_name=AppName, app_state=AppState}) ->
	{ok, NewAppState, Response}=AppName:handle_request(Request, AppState),
	client:respond(Response).
	{noreply, State#state{app_state=NewAppState}};
handle_cast(_Msg, _State) ->
	{noreplay, _State}.

handle_call(get_state, State) ->
	{reply, app_state, State};
handle_call(_Msg, _State) ->
	Reply = _Msg,
	{reply, Reply, _State}.

handle_info(_Info, _State) ->
	{noreply, _State}.

code_change(OldVsn, State, Extra) ->
	{ok, State}.

terminate(Reason, State) ->
	ok.

%%%--------------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------------

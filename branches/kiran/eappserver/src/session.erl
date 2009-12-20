%% Author: kiran
%% Created: Aug 8, 2009
%% Description: TODO: Add description to ps_session
-module(_session).

%%
%% Include files
%%
-include("server_records.hrl").
-include("common.hrl").
%%
%% Exported Functions
%%
-export([delete/1, validate/1, create/0, init/0]).

%%
%% API Functions
%%
%%
%% TODO: Add description of init/function_arity
%%
init() ->
	ok.

%%
%% TODO: Add description of delete/function_arity
%%
delete(_Arg0) -> 
	ok.
%%
%% TODO: Add description of validate/function_arity
%%
validate(ClientRec) ->
	case ps_data_manager:get_pid(ClientRec#client_info.session) of
		{ok, no_session} ->
			%%session not present
			ok;
		{ok, Session} ->
			%%session present
			ok
	end.
%%
%% TODO: Add description of create/function_arity
%%
create() ->
	ok.


%%
%% Local Functions
%%


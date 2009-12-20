%%% -------------------------------------------------------------------
%%% Author  : kiran
%%% Description :Parses the request and sends it to client process in
%%%		erlang format for processing
%%%
%%% Created : Aug 6, 2009
%%% -------------------------------------------------------------------
-module(request).
-include("server_records.hrl").
-include("common.hrl").
-export([handle/3]).
%% ====================================================================
%% External functions
%% ====================================================================
handle(Socket, SocketType, Req) ->
	spawn(?MODULE, handle_req, [SocketType, Socket, Req]),
	{ok}.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% {req,[{session,"3453454"}],
%%     [{app,[{name,"appname"},{app_req,"reqstring"}],[]}]}

handle_req(tcp, Socket, {request, Params, ReqList}) ->
	Client = get_client_info(Params, #client_info{}),
	Session = validate_session(Client),
	ReqInfoList = get_req_info_list(ReqList, []),
	client:handle_request(Session, ReqInfoList),
	{ok, succ};
handle_req(http, Socket, Req) ->
	{ok, succ};
handle_req(_ReqType, _OtherSocket, _Req) ->
	?PRINT({_ReqType, _OtherSocket, _Req}),
	{ok, fail}.
%This function is not required as of now
process_request([{request, Params, []}|RestReq]) ->
	process_request(RestReq);
process_request([]) ->
	ok.

%% Function: get_client_info/2
%% Description: extracts the client info from the parsed xml request.
%% Return: #client_info{} which contains all the client info required by server
get_client_info([{user_agent, UserAgent}|RestInfo], ClientInfo) ->
	NewClientInfo = ClientInfo#client_info{user_agent = UserAgent},
	get_client_info(RestInfo, NewClientInfo);
get_client_info([{session, Session}|RestInfo], ClientInfo) ->
	NewClientInfo = ClientInfo#client_info{session = Session};
get_client_info([], ClientInfo) ->
	ClientInfo.

%% Function: get_req_info_list/2
%% Description: Extracts the request body of list of multiple requests
%%		from parsed xml requests.
%% Return: {ok, RequestRecordList}
%%		RequestRecordList -> [#client_req{}]
get_req_info_list([H|RestReq], ReqInfoList) ->
	get_req_info_list(RestReq, [get_req_info(H, #client_req{})|ReqInfoList]);
get_req_info_list([], ReqInfoList) ->
	{ok, ReqInfoList}.

%{type, subtype, id, pass, lt, lg}
get _req_info([{type, Type}|RestReq], ClientReq) ->
	NewClientReq = ClientReq#client_req{type = Type},
	get_req_info(RestReq, NewClientReq);
get_req_info([{subtype, SubType}|RestReq], ClientReq) ->
	NewClientReq = ClientReq#client_req{subtype = SubType},
	get_req_info(RestReq, NewClientReq);
get_req_info([{id, ID}|RestReq], ClientReq) ->
	NewClientReq = ClientReq#client_req{id = ID},
	get_req_info(RestReq, NewClientReq);
get_req_info([{pass, Pass}|RestReq], ClientReq) ->
	NewClientReq = ClientReq#client_req{pass = decrypt_pass(Pass)},
	get_req_info(RestReq, NewClientReq);
get_req_info([{lt, LT}|RestReq], ClientReq) ->
	NewClientReq = ClientReq#client_req{lt = LT},
	get_req_info(RestReq, NewClientReq);
get_req_info([{lg, LG}|RestReq], ClientReq) ->
	NewClientReq = ClientReq#client_req{lt = LG},
	get_req_info(RestReq, NewClientReq);
get_req_info([], ClientReq) ->
	{ok, ClientReq}.

validate_session(Client) ->
	Session = "1234",
	{ok, Session}.

decrypt_pass(Pass) ->
	{ok, Pass}.

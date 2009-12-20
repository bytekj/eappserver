%%% -------------------------------------------------------------------
%%% Author  : kiran
%%% Description :
%%%
%%% Created : Jul 24, 2009
%%% -------------------------------------------------------------------
-module(_socket).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("common.hrl").
-include("server_records.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start/1, send_cast/2, send_call/2, send_info/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket}).

%% ====================================================================
%% External functions
%% ====================================================================
%%
%% TODO: Add description of start/function_arity
%%
start(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).

send_cast(Pid, Msg) ->
	gen_server:cast(Pid, Msg).

send_call(Pid, Msg) ->
	gen_server:call(Pid, Msg).

send_info(Pid, Msg) ->
	Pid!Msg.
%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Socket]) ->
    {ok, #state{socket = Socket}, hibernate}.
	
%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({send, Data}, _From, #state{socket = Socket}) ->
	case gen_tcp:send(Socket, erlang:list_to_binary(Data)) of
		ok ->
			gen_tcp:close(Socket),
			ok;
		{error, Reason} ->
			?PRINT(Reason)
	end,
	{reply, ok, stop};
handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State, hibernate}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({recv, tcp}, State=#state{socket = Socket}) ->
	handle_tcp(Socket),
	{noreply, State, hibernate};
handle_cast({recv, http}, State=#state{socket = Socket}) ->
	handle_http(Socket),
	{noreply, State, hibernate};
handle_cast(Msg, State) ->
	?PRINT({Msg}),
    {noreply, State, hibernate}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(Info, State) ->
    {noreply, State, hibernate}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

send(Pid, Data) ->
	gen_server:call(Pid, {send, Data}).

handle_http(Socket) ->
	Req = request(Socket),
	gen_tcp:send(Socket, add_header("success")),
	ok.
	
request(Socket) ->
    	case gen_tcp:recv(Socket, 0, 30000) of
    	{ok, {http_request, Method, {abs_path,Path}, Version}} ->
		?PRINT({Method, Path, Version}),
		headers(Socket,#req{path = Path}, #header{});
    	{error, {http_error, "\r\n"}} ->
		request(Socket);
	{error, {http_error, "\n"}} ->
            	request(Socket);
	_Other ->
		io:format("~p~n", [_Other]),
	    	exit(normal)
    end.

headers(Socket, Req, H) ->
	case gen_tcp:recv(Socket, 0, 30000) of
		{ok, {http_header, _, 'Content-Length', _, Val}} ->
    		Len = list_to_integer(Val),
	    	headers(Socket, Req, H#header{content_length = Len});
    	{ok, {http_header, _, 'Connection', _, Val}} ->
        	headers(Socket, Req, H#header{connection=Val});
	{ok, {http_header, _, 'Cookie', _, Val}} ->
		headers(Socket, Req#req{session = Val}, H#header{cookie = Val}); 
	{ok, {http_header, _, 'Referer', _, Val}} ->
		headers(Socket, Req#req{referer = Val}, H#header{referer = Val});
	{ok, {http_header, _, Header, _, Val}} ->
            	headers(Socket, Req, H);
	{error, {http_error, "\r\n"}} ->
		headers(Socket, Req, H);
	{error, {http_error, "\n"}} ->
            	headers(Socket, Req, H);
    	{ok, http_eoh} -> 
            	Req#req{headers = H, socket = Socket};
	_Other ->
	    	exit(normal)
    end.

add_header(Response) ->
	Status = "HTTP/1.1 200 OK\r\n",
	Server = "Server: Erlang\r\n",
	ContentType = "Content-Type: text/html\r\n",
	Connection = "Connection: close\r\n",
	Content_Length = "Content-Length: " ++ integer_to_list(iolist_size(Response)) ++ "\r\n",
	Header = Status ++ Server ++ ContentType ++ Connection ++ Content_Length ++ "\r\n" ++ Response.


handle_tcp(Socket) ->
	inet:setopts(Socket, [{active, once}]),
	receive
	{tcp, Socket, Binary} ->
		%handle_xml_request(erlang:binary_to_list(Binary));
		?PRINT(Binary),
		gen_tcp:send(Socket, Binary),
		gen_tcp:close(Socket);
	{tcp_closed,Socket } ->
		?PRINT("Socket closed");
	_Other ->
		?PRINT(_Other)
	end,
	?PRINT(Socket).

handle_xml_request(Req) ->
	{Xml, _Rest} = xmerl_scan:string(Req, [{space, normalize}, {encoding, "utf-8"}]),
	SimpleReq = xmerl_lib:simplify_element(Xml),
	request:handle(self(), tcp, SimpleReq).
	


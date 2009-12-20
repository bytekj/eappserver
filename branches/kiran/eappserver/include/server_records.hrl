-record(req, {headers, method, path, session=0, socket, referer}). 
-record(header,{
		accept,	%Content-Types that are acceptable 	Accept: text/plain
		accept_charset, 	%Character sets that are acceptable 	Accept-Charset: iso-8859-5
		accept_encoding, 	%Acceptable encodings 	Accept-Encoding: compress, gzip
		accept_language, 	%Acceptable languages for response 	Accept-Language: da
		accept_ranges, 	%Allows the server to indicate its acceptance of range requests for a resource 	Accept-Ranges: bytes
		authorization, 	%Authentication credentials for HTTP authentication 	Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==
		cache_control, 	%Used to specify directives that MUST be obeyed by all caching mechanisms along the request/response chain 	Cache-Control: no-cache
		connection, 	%What type of connection the user-agent would prefer 	Connection: close
		content_length, 	%The length of the request body in 8-bit bytes 	Content-Length: 348
		content_type, 	%The mime type of the body of the request (used with POST and PUT requests) 	Content-Type: application/x-www-form-urlencoded
		cookie, 	%an HTTP cookie previously sent by the server with Set-Cookie (below) 	Cookie: $Version=1; UserId=JohnDoe
		date, 	%The date and time that the message was sent 	Date: Tue, 15 Nov 1994 08:12:31 GMT
		expect, 	%Indicates that particular server behaviors are required by the client 	Expect: 100-continue
		from, 	%The email address of the user making the request 	From: user@email.com
		host, 	%The domain name of the server (for virtual hosting), mandatory since HTTP/1.1 	Host: en.wikipedia.org
		if_match, 	%Only perform the action if the client supplied entity matches the same entity on the server. This is mainly for methods like PUT to only update a resource if it has not been modified since the user last updated it. 	If-Match: "737060cd8c284d8af7ad3082f209582d"
		if_modified_since, 	%Allows a 304 Not Modified to be returned if content is unchanged 	If-Modified-Since: Sat, 29 Oct 1994 19:43:31 GMT
		if_none_match, 	%Allows a 304 Not Modified to be returned if content is unchanged, see HTTP ETag 	If-None-Match: "737060cd8c284d8af7ad3082f209582d"
		if_range, 	%If the entity is unchanged, send me the part(s) that I am missing; otherwise, send me the entire new entity 	If-Range: "737060cd8c284d8af7ad3082f209582d"
		if_unmodified_since, 	%Only send the response if the entity has not been modified since a specific time. 	If-Unmodified-Since: Sat, 29 Oct 1994 19:43:31 GMT
		max_forwards, 	%Limit the number of times the message can be forwarded through proxies or gateways. 	Max-Forwards: 10
		pragma, 	%Implementation-specific headers that may have various effects anywhere along the request-response chain. 	Pragma: no-cache
		proxy_authorization, 	%Authorization credentials for connecting to a proxy. 	Proxy-Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==
		range, 	%Request only part of an entity. 	Range: bytes=500-999
		referer, 	%This is the address of the previous web page from which a link to the currently requested page was followed. 	Referer: http://en.wikipedia.org/wiki/Main_Page
		tE, 	%The transfer encodings the user agent is willing to accept: the same values as for the response header Transfer-Encoding can be used, plus the "trailers" value (related to the "chunked" transfer method) to notify the server it accepts to receive additional headers (the trailers) after the last, zero-sized, chunk. 	TE: trailers, deflate;q=0.5
		upgrade, 	%Ask the server to upgrade to another protocol. 	Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11
		user_agent, 	%The user agent string of the user agent 	User-Agent: Mozilla/5.0 (Linux; X11)
		via, 	%Informs the server of proxies through which the request was sent. 	Via: 1.0 fred, 1.1 nowhere.com (Apache/1.1)
		warn 	%A general warning about possible problems with the entity body. 	Warn: 199 Miscellaneous warning
		}).

-record(client_info, {user_agent, session}).
-record(client_req, {type, subtype, id, pass, lt, lg}).

-module(test).
-compile(export_all).
start(N) ->
	loop(N).
loop(0) ->
	ok;
loop(N) ->
	io:format("kiran"),
	loop(N-1).

test_xml_request() ->
	Req=	"<req session=\"3453454\">"++
			"<app name=\"appname\">"++
				"<request data=\"something\"/>"++
			"</app>"++
			"<app name=\"appname1\">"++
				"<request data=\"something else\"/>"++
			"</app>"++
		"</req>",
	{Xml, _Rest} = xmerl_scan:string(Req, [{space, normalize}, {encoding, "utf-8"}]),
	xmerl_lib:simplify_element(Xml).
	

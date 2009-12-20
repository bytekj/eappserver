-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.
-define(PRINT(MSG), io:format("~n++++++++++INFO++++++++++"),
		    io:format("~n~p", [MSG]),
		    io:format("~n++++++++++++++++++++++++"),
		    MSG).

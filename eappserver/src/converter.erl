%%Author : Kiran Khaladkar
%%The module is used to do conversions from xml to records and vice versa
-module(converter).
-export([test_record/0, test/0]).
-record(address, {room=7, building="Dewki Wadi", city="Mumbai", pincode=400016}).
-record(contact, {name="Kiran", address = #address{}, surname="Khaladkar"}).
test() ->
	working.

test_record() ->
	I = get_rec_info(contact),
	%erlang:tuple_to_list(#contact{}).
	rec_to_xml(#contact{}).
	%lists:flatten(xmerl:export_simple(convert_to_xml(V, I), xmerl_xml, [{prolog, []}])).

append_to_term({Name, Tags, Vals},Tag, Val ) when is_tuple(Val)->
	io:format("complex~n"),
	{Name, Tags, [rec_to_term(tuple_to_list(Val))|Vals]};

append_to_term({Name, Tags, Vals}, Tag, Val) ->
	io:format("simple~n"),
	{Name, lists:append(Tags, [{Tag, Val}]), Vals}.

rec_to_term([Rec_Name|Rec_Vals]) ->
	I = get_rec_info(Rec_Name),
	append({Rec_Name, [], []}, I, Rec_Vals).

append(Term, [], []) ->
	Term;

append(Term, [Tag|Rest_Tags], [Val|Rest_Vals]) ->
	append(append_to_term(Term, Tag, Val), Rest_Tags, Rest_Vals).

rec_to_xml(Rec) ->
	Term = rec_to_term(tuple_to_list(Rec)),
	lists:flatten(xmerl:export_simple([Term], xmerl_xml, [{prolog, []}])).

%Reason behind writing these functions is record_info is expanded 
%compile time so we cant pass a variable to it
get_rec_info(address) ->
	record_info(fields, address);
get_rec_info(contact) ->
	record_info(fields, contact).

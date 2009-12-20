-module(record_info).
-export([get/1]).
%% get(user_codes) ->
%% 	record_info(fields, user_codes);
%% get(user_profiles) ->
%% 	record_info(fields, user_profiles);
%% get(buddy_list) ->
%% 	record_info(fields, buddy_list);
%% get(image) ->
%% 	record_info(fields, image);
%% get(user_blog) ->
%% 	record_info(fields, user_blog);
%% get(blog_content) ->
%% 	record_info(fields, blog_content);
%% get(location) ->
%% 	record_info(fields, location);
%% get(presence) ->
%% 	record_info(fields, presence);
%% get(inverse_buddy_list) ->
%% 	record_info(fields, inverse_buddy_list);
%% get(sessions) ->
%% 	record_info(fields, sessions);
get(Other) ->
	{not_found}.

